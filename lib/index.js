'use strict'; var fs = require('fs');
var async = require('async');
var path = require('path');
var esprima = require('esprima');
var estraverse = require('estraverse');
var _ = require('lodash');
var resolve = require('resolve');

exports = module.exports = findDefinition;

function findDefinition(fp, line, col, nodeModulesFp, coreModulesFp, cb) {
  fs.readFile(fp, function(err, file) {
    if(err) return cb(err);

    var ast = esprima.parse(file.toString(), {
      loc: true,
    });
    ast.path = path.resolve(fp);

    var node = findByLoc(ast, line, col);
    if(node) {
      var name = getName(node);
      if(!name) {
        err = new Error(JSON.stringify({
          error: 'Invalid token',
          token: node,
        }));
        return cb(err);
      }

      return findDefinitionRecursive(
        fp, nodeModulesFp, coreModulesFp,
        ast, name, cb
      );
    }

    cb(new Error('Couldn\'t find token'));
  });
}

function getModuleAst(fp, nodeModulesFp, coreModulesFp, moduleName, cb) {
  if(resolve.core[moduleName]) {
    var modulePath = path.join(coreModulesFp, moduleName + '.js');
    fs.readFile(modulePath, function(err, file) {
      if(err) return cb(err);
      var ast = esprima.parse(file.toString(), { loc: true });
      cb(null, _.extend(ast, { path: modulePath, }));
    });
    return;
  }

  var opts = {
    moduleDirectory: nodeModulesFp,
    basedir: path.dirname(fp),
  };

  return resolve(moduleName, opts, function(err, modulePath) {
    if(err) return cb(err);

    return fs.readFile(modulePath, function(err, file) {
      if(err) return cb(err);

      var ast = esprima.parse(file.toString(), { loc: true, });
      cb(null, _.extend(ast, { path: modulePath }));
    });
  });
}
exports.getModuleAst = getModuleAst;

function findDefinitionRecursive(fp, nodeModulesFp, coreModulesFp, ast, name, cb) {
  var def = findDefinitionInAst(ast, name);

  if(def) {
    if(def.init && def.init.callee && def.init.callee.name === 'require') {
      getModuleAst(fp, nodeModulesFp, coreModulesFp, def.init.arguments[0].value,
        function(err, modAst) {
          if(err) return cb(err);

          var exportedName = name.split('.');
          exportedName[0] = 'exports';
          exportedName = exportedName.join('.');

          function makeFind(name) {
            return function(cb) {
              findDefinitionRecursive(
                modAst.path,
                nodeModulesFp,
                coreModulesFp,
                modAst,
                name,
                function(err, name) {
                  cb(null, name);
                }
              );
            };
          }

          async.parallel([
            makeFind(exportedName),
            makeFind(name),
          ], function(err, defs) {
            var def = _.find(defs, function(def) { return !!def; });
            if(!def) return cb(new Error('Failed to find ' + name));
            cb(null, def);
          });
      });
      return;
    }

    def.path = ast.path;
    return cb(null, def);
  }

  cb(new Error('Failed to find `' + name +'` definition'));
}
exports.findDefinitionRecursive = findDefinitionRecursive;

function findModuleNameInAst(ast, name) {
  return findInAst(ast, function(node) {
    if(node.type === 'VariableDeclaration') {
      var dec = _.find(node.declarations, function(dec) {
        return dec.init && dec.init.type === 'CallExpression' &&
               dec.id.name == name && dec.init.callee.name === 'require';
      });

      if(dec) {
        return dec.init.arguments[0].value;
      }
    }
  });
}
exports.findModuleNameInAst = findModuleNameInAst;

function findDefinitionInAst(ast, name) {
  var splitName = name.split('.');

  if(splitName.length > 1) {
    return findPropertyAssignment(ast, splitName);
  }

  return findInAst(ast, function(node) {
    if(node.type === 'FunctionDeclaration') {
      if(node.id.name === name) {
        return node;
      }
    } else if(node.type === 'VariableDeclaration') {
      var m = _.find(node.declarations, function(dec) {
        return dec.id.name === name;
      });

      if(m) {
        return m;
      }
    } else if(node.type === 'ExpressionStatement' &&
              node.expression.type === 'AssignmentExpression') {
      var assignmentName = assignmentExpressionToSplitName(node);
      if(node.name === name) {
        return node;
      } else if(assignmentName[0] === name) {
        return node;
      } else if(node.id && node.id.name === name) {
        return node;
      }
    }
  });
}
exports.findDefinitionInAst = findDefinitionInAst;

function findPropertyAssignment(ast, splitName) {
  var matches = [];
  var nameDepth = splitName.length - 1;

  function findTargetProp(props, depth) {
    if(!props) return;
    for(var i = 0, len = props.length; i < len; i++) {
      var prop = props[i];
      var m = isTargetProp(prop, depth);
      if(m) {
        return m;
      }
    }
  }

  function isTargetProp(prop, depth) {
    if(prop.key.name !== splitName[depth]) return false;
    if(depth >= nameDepth) {
      prop.score = nameDepth;
      return prop;
    }

    var propPrime = findTargetProp(prop.value.properties, depth + 1);
    if(propPrime) return propPrime;

    prop.score = depth;
    return prop;
  }

  findInAst(ast, function(node) {
    var m;

    if(node.type === 'AssignmentExpression') {
      var assignmentName = assignmentExpressionToSplitName(node);
      var score = nequals(assignmentName, splitName);

      if(score) {
        node.score = score;
        matches.push(node);
      }
    } else if(node.type === 'VariableDeclaration') {
      for(var i = 0, len = node.declarations.length; i < len; i++) {
        var dec = node.declarations[i];

        if(dec.id.name === splitName[0] && dec.init) {
          m = findTargetProp(dec.init.properties, 1);
          if(m) matches.push(m);
          else {
            dec.score = 0;
            matches.push(dec);
          }
        }
      }
    }
  });

  var sorted = _.sortBy(matches, 'score');
  return _.last(sorted);
}
exports.findPropertyAssignment = findPropertyAssignment;

function assignmentExpressionToSplitName(assignment) {
  var currNode = assignment.left;
  var splitName = [];
  while(true) {
    if(!currNode || !(currNode.name || (currNode.property && currNode.property.name))) {
      break;
    }

    splitName.push(currNode.name || currNode.property.name);
    currNode = currNode.object;
  }
  return splitName.reverse();
}
exports.assignmentExpressionToSplitName = assignmentExpressionToSplitName;

function nequals(arr1, arr2) {
  var smaller = arr1.length > arr2.length ? arr2 : arr1;
  var bigger  = smaller === arr1 ? arr2 : arr1;
  for(var i = 0, len = smaller.length; i < len; i++) {
    if(smaller[i] !== bigger[i]) break;
  }
  return i;
}

function getName(node) {
  switch(node.type) {
  case 'MemberExpression':
    return node.object.name + '.' + getName(node.property);
  case 'Identifier':
    return node.name;
  case 'ExpressionStatement':
    switch(node.expression.type) {
    case 'AssignmentExpression':
      return assignmentExpressionToSplitName(node.expression).join('.');
    default:
      return node.name || (node.expression && node.expression.callee.name);
    }
    break;
  default:
    return node.name || (node.expression && node.expression.callee.name);
  }
}
exports.getName = getName;

function findInAst(ast, fn) {
  var targetNode;

  estraverse.traverse(ast, {
    enter: function(node) {
      targetNode = fn(node);
      if(targetNode) {
        this.break();
      }
    },
  });

  return targetNode;
}
exports.findInAst = findInAst;

function findByLoc(ast, line, col) {
  var targetTypes = ['ExpressionStatement', 'MemberExpression'];
  return findInAst(ast, function(node) {
    if(node.loc &&
       withinBounds(node.loc.start.line, node.loc.end.line, line) &&
       withinBounds(node.loc.start.column, node.loc.end.column, col) &&
       _.includes(targetTypes, node.type)) {
      return node;
    }
  });
}
exports.findByLoc = findByLoc;

function withinBounds(start, end, x) {
  return x >= start && x <= end;
}
