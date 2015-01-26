'use strict';
var fs = require('fs');
var esprima = require('esprima');
var estraverse = require('estraverse');
var _ = require('lodash');

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
    }
  });
}
exports.findDefinitionInAst = findDefinitionInAst;

function findPropertyAssignment(ast, splitName) {
  var nameDepth = splitName.length - 1;
  function findTargetProp(props, depth) {
    for(var i = 0, len = props.length; i < len; i++) {
      var prop = props[i];
      var m = isTargetProp(prop, depth);
      if(m) {
        return m;
      }
    }
  }

  function isTargetProp(prop, depth) {
    if(prop.key.name === splitName[depth]) {
      if(depth < nameDepth) {
        if(prop.value.properties.length) {
          return findTargetProp(prop.value.properties, depth + 1);
        }
        return false;
      }
      return prop;
    }
    return false;
  }

  return findInAst(ast, function(node) {
    var m;

    if(node.type === 'AssignmentExpression') {
      m = node.left;
      for(var depth = nameDepth; depth > -1; depth--) {
        if(!m || (m.name || m.property.name) !== splitName[depth]) {
          m = false;
          break;
        }

        m = m.object || m;
      }

      if(m) return m;
    } else if(node.type === 'VariableDeclaration') {
      for(var i = 0, len = node.declarations.length; i < len; i++) {
        var dec = node.declarations[i];

        if(dec.id.name === splitName[0]) {
          m = findTargetProp(dec.init.properties, 1);
          if(m) return m;
        }
      }
    }
  });
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
