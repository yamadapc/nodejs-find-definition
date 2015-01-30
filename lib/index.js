'use strict';
var fs = require('fs');
var path = require('path');
var esprima = require('esprima');
var estraverse = require('estraverse');
var _ = require('lodash');
var resolve = require('resolve');

exports = module.exports = findDefinition;

function findDefinition(fp, line, col, nodeModulesFp, cb) {
  fs.readFile(fp, function(err, file) {
    if(err) return cb(err);

    var ast = esprima.parse(file.toString(), {
      loc: true,
    });


    var node = findByLoc(ast, line, col);
    if(node) {
      // This needs fixing
      var definition = findDefinitionInAst(ast, node.expression.callee.name);

      return cb(null, definition);
    }

    cb(null, null);
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
    basedir: fp,
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
          findDefinitionRecursive(modAst.path, nodeModulesFp, coreModulesFp, modAst, name, cb);
      });
      return;
    }
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
      console.log('AssignmentExpression');
      console.log(node);
      if(node.id.name === name) {
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

    //node.left && node.left.property && console.log(node.left.property.name, splitName[nameDepth]);
    if(node.type === 'AssignmentExpression' && node.left.property &&
       node.left.property.name === splitName[nameDepth]) {
      m = node.left.object;

      for(var depth = nameDepth - 1; depth > -1; depth--) {
        if(!m || (m.name || m.property.name) !== splitName[depth]) {
          m = false;
          break;
        }

        m = m.object || m;
      }

      if(m) {
        m.score = Math.abs(depth - nameDepth);
        matches.push(m);
      } else {
        node.score = 0;
        matches.push(node);
      }
    } else if(node.type === 'VariableDeclaration') {
      for(var i = 0, len = node.declarations.length; i < len; i++) {
        var dec = node.declarations[i];

        if(dec.id.name === splitName[0]) {
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
  return findInAst(ast, function(node) {
    if(node.loc &&
       withinBounds(node.loc.start.line, node.loc.end.line, line) &&
       withinBounds(node.loc.start.column, node.loc.end.column, col)) {
      return node;
    }
  });
}
exports.findByLoc = findByLoc;

function withinBounds(start, end, x) {
  return start >= x && x <= end;
}
