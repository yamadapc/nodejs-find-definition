'use strict';
function wrapper() {
function doSomething() {
}

doSomething();

var obj = {
  something: 'here',
  there: function() {},
  nested: {
    here: 'something',
  },
};

exports.something = function something() {
};

exports.really = {};
exports.really.fucking = { nested: {} };

exports.really.fucking.nested.here = 'hehe';

var fs = require('fs');

var something = require('./something/there');




var readFile = fs.readFile;

var WSProt = fs.WriteStream.prototype;
}
