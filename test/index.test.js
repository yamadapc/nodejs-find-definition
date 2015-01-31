'use strict'; /* global describe, it, before */
var path = require('path');
var fs = require('fs');
var esprima = require('esprima');
var _ = require('lodash');
var should = require('should');
var findDefinition = require('..');

describe('find-definition', function() {
  it('gets exposed', function() {
    should.exist(findDefinition);
    findDefinition.should.be.instanceof(Function);
  });

  before(function() {
    this.testModuleFp = path.join(__dirname, 'test-module.js');
    this.nodeModulesFp = path.join(__dirname, '../node_modules');

    this.testModule = fs.readFileSync(this.testModuleFp);
    this.testAst = esprima.parse(this.testModule, { loc: true });
  });

  describe('findDefinition(fp, line, col, nodeModulesFp, cb)', function() {
    describe('when finding the definition of file local variables', function() {
      it('returns the correct position for the definition', function(done) {
        var fp = this.testModuleFp;
        var nmfp = this.nodeModulesFp;

        findDefinition(fp, 6, 0, nmfp, function(err, result) {
          result.loc.start.should.eql({
            line: 3,
            column: 0
          });
          done();
        });
      });
    });

  });

  describe('.getModuleAst(fp, nodeModulesFp, moduleName)', function() {
    it('returns the AST for a given local module', function(done) {
      var _this = this;

      findDefinition.getModuleAst(__dirname, (__dirname + '../core'), '../node_modules', './test-module', function(err, ast) {
        should.not.exist(err);
        should.exist(ast);
        ast.should.have.properties(['type', 'body', 'loc', 'path']);
        ast.path.should.equal(_this.testModuleFp);
        done();
      should.not.exist(err);
      });
    });
  });

  describe('.findDefinitionRecursive(ast, name, cb)', function() {
    function all(actions, cb) {
      cb = _.once(cb);
      var results = [];
      var nactions = actions.length;
      var done = 0;

      function iter(i, err, result) {
        if(err) return cb(err);

        results.push(result);
        done++;

        if(done >= nactions) {
          cb(null, results);
        }
      }

      _.each(actions, function(action, i) {
        action(_.partial(iter, i));
      });
    }

    function testArgs(argss, cb) {
      return all(_.map(argss, function(args) {
        return function(cb) {
          args.input.push(function(err, result) {
            if(err) return cb(err);

            try {
              args.testResult(result);
            } catch(err) {
              return cb(err);
            }

            cb(null);
          });

          findDefinition.findDefinitionRecursive.apply(null, args.input);
        };
      }), cb);
    }

    it('recursivelly searches modules for the definition of a name', function(done) {
      testArgs([
        {
          input: [
            __dirname,
            '../node_modules',
            __dirname + '/../core',
            this.testAst,
            'fs.WriteStream',
          ],
          testResult: function(def) {
            def.loc.start.line.should.equal(1732);
          },
        },
        {
          input: [
            __dirname,
            '../node_modules',
            __dirname + '/../core',
            this.testAst,
            'fs.WriteStream.prototype',
          ],
          testResult: function(def) {
            def.loc.start.line.should.equal(1732);
          },
        },
      ], done);
    });
  });

  describe('.findModuleNameInAst(ast, name)', function() {
    it('finds the module from which a name comes from', function() {
      var name = findDefinition.findModuleNameInAst(this.testAst, 'fs');
      should.exist(name);
      name.should.equal('fs');
      name = findDefinition.findModuleNameInAst(this.testAst, 'something');
      name.should.equal('./something/there');
    });
  });

  describe('.assignmentExpressionToSplitName(assignment)', function() {
    it('gets "path" to a property given an assignment expression', function() {
      var node = esprima.parse('obj = { property: {} }; obj.property.here = "something";');
      findDefinition.assignmentExpressionToSplitName(node.body[1].expression)
        .should.eql(['obj', 'property', 'here']);
    });
  });

  describe('.findByLoc(ast, line, col)', function() {
    it('returns the node at the location provided', function() {
      var testModule = fs.readFileSync(path.join(__dirname, 'test-module.js'))
        .toString();
      var ast = esprima.parse(testModule, { loc: true });

      var node = findDefinition.findByLoc(ast, 6, 0);
      should.exist(node);
      node.type.should.equal('ExpressionStatement');
      node.expression.type.should.equal('CallExpression');
      node.expression.callee.name.should.equal('doSomething');
    });
  });

  describe('.findDefinitionInAst(ast, name)', function() {
    it('finds variable declarations in the parsed module', function() {
      var ast = this.testAst;
      var node = findDefinition.findDefinitionInAst(ast, 'obj');
      should.exist(node);
      node.type.should.equal('VariableDeclarator');
      node.loc.start.should.eql({
        line: 8,
        column: 4,
      });
    });

    it('finds function declarations in the parsed module', function() {
      var ast = this.testAst;
      var node = findDefinition.findDefinitionInAst(ast, 'doSomething');
      should.exist(node);
      node.type.should.equal('FunctionDeclaration');
      node.loc.start.should.eql({
        line: 3,
        column: 0,
      });
    });

    it('finds object property assignments in the parsed module', function() {
      var ast = this.testAst;
      var node = findDefinition.findDefinitionInAst(ast, 'obj.something');
      should.exist(node);
      node.type.should.equal('Property');
      node.loc.start.should.eql({
        line: 9,
        column: 2,
      });
    });

    it('finds nested object property assignments in the parsed module', function() {
      var ast = this.testAst;
      var node = findDefinition.findDefinitionInAst(ast, 'obj.nested.here');
      should.exist(node);
      node.type.should.equal('Property');
      node.loc.start.should.eql({
        line: 12,
        column: 4,
      });

      node = findDefinition.findDefinitionInAst(ast, 'exports.really.fucking.nested.here');
      should.exist(node);
      //node.type.should.equal('MemberExpression');
      node.loc.start.should.eql({
        line: 22,
        column: 0,
      });
    });
  });
});
