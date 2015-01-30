'use strict'; /* global describe, it, before */
var path = require('path');
var fs = require('fs');
var esprima = require('esprima');
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
    it('recursivelly searches modules for the definition of a name', function(done) {
      findDefinition.findDefinitionRecursive(
        __dirname,
        '../node_modules',
        __dirname + '/../core',
        this.testAst,
        'fs.WriteStream',
        function(err, def) {
          if(err) return done(err);
          def.loc.start.line.should.equal(1732);
          done();
        }
      );
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
