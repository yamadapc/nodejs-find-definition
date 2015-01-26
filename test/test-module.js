'use strict';

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
