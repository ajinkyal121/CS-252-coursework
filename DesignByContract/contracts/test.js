var assert = require('assert');

// Perhaps remove this when it is part of a module

var {
  isNumber: isNumber,
  isBoolean: isBoolean,
  isDefined: isDefined,
  isString: isString,
  isNegative: isNegative,
  isPositive: isPositive,
  and: and,
  or: or,
  not: not,
  contract: contract
} = require('./contracts.js');




// Testing contracts

describe('isNumber()', function() {
  it('should return true when its argument is a number', function() {
    assert.equal(isNumber(3), true);
    assert.equal(isNumber(99), true);
    assert.equal(isNumber(0), true);
    assert.equal(isNumber(-1), true);
  });
  it('should return false when its argument is not a number', function() {
    assert.equal(isNumber("hello"), false);
    assert.equal(isNumber(undefined), false);
    assert.equal(isNumber(null), false);
    assert.equal(isNumber(true), false);
    assert.equal(isNumber({}), false);
    assert.equal(isNumber([]), false);
    assert.equal(isNumber(function(){}), false);
  });
});


describe('isBoolean()', function() {
  it('should return true when the argument is a boolean', function() {
    assert.equal(isBoolean(true), true);
    assert.equal(isBoolean(false), true);
  });
  it('should return false when the argument is anything else', function() {
    assert.equal(isBoolean(0), false);
    assert.equal(isBoolean('hello'), false);
    assert.equal(isBoolean('true'), false);
    assert.equal(isBoolean({}), false);
    assert.equal(isBoolean(null), false);
    assert.equal(isBoolean(undefined), false);
  });
});

describe('isDefined()', function() {
  it('should return false when the argument is null or undefined', function() {
    assert.equal(isDefined(null), false);
    assert.equal(isDefined(undefined), false);
  });
  it('should return true when the argument is anything else', function() {
    assert.equal(isDefined('null'), true);
    assert.equal(isDefined('true'), true);
    assert.equal(isDefined(''), true);
    assert.equal(isDefined(42), true);
    assert.equal(isDefined(0), true);
    assert.equal(isDefined(false), true);
    assert.equal(isDefined(true), true);
  });
});

describe('isString()', function() {
  it('should return true if the argument is a string literal', function() {
    assert.equal(isString(''), true);
    assert.equal(isString('I'), true);
    assert.equal(isString('hello'), true);
    assert.equal(isString('I am the very model of a modern major general'), true);
  });
  it('should return true if the argument is a string object', function() {
    assert.equal(isString(new String('LlamÃ© una vez a la visiÃ³n y vino.')), true);
    assert.equal(isString(new String("Y era pÃ¡lida y triste,")), true);
    assert.equal(isString(new String("y sus pupilas ardÃ­an como hogueras de martirios.")), true);
  });
  it('should return false if the argument is anything else', function() {
    assert.equal(isString(42), false);
    assert.equal(isString(null), false);
    assert.equal(isString(undefined), false);
    assert.equal(isString({}), false);
    assert.equal(isString(true), false);
  });
});

describe('isNegative()', function() {
  it('should return true when its argument is a negative number', function() {
    assert.equal(isNegative(-3), true);
    assert.equal(isNegative(-99), true);
    assert.equal(isNegative(-1), true);
  });
  it('should return false when its argument is 0 or a positive number', function() {
    assert.equal(isNegative(0), false);
    assert.equal(isNegative(3), false);
    assert.equal(isNegative(99), false);
  });
  it('should return false if the argument is anything else', function() {
    assert.equal(isNegative("hello"), false);
    assert.equal(isNegative(null), false);
    assert.equal(isNegative(undefined), false);
    assert.equal(isNegative({}), false);
    assert.equal(isNegative(true), false);
  });
});

describe('isPositive()', function() {
  it('should return true when its argument is a positive number', function() {
    assert.equal(isPositive(3), true);
    assert.equal(isPositive(99), true);
    assert.equal(isPositive(1), true);
  });
  it('should return false when its argument is 0 or a negative number', function() {
    assert.equal(isPositive(0), false);
    assert.equal(isPositive(-3), false);
    assert.equal(isPositive(-99), false);
  });
  it('should return false if the argument is anything else', function() {
    assert.equal(isPositive("hello"), false);
    assert.equal(isPositive(null), false);
    assert.equal(isPositive(undefined), false);
    assert.equal(isPositive({}), false);
    assert.equal(isPositive(true), false);
  });
});

describe('not()', function () {
  it('should return true when the original contract was false', function() {
    assert.equal(not(isDefined)(null), true);
    assert.equal(not(isPositive)(0), true);
  });
  it('should return false when the original contract was true', function() {
    assert.equal(not(isDefined)("hello"), false);
    assert.equal(not(isPositive)(42), false);
  });
  it('should return a function with an "expected" property set to something informative', function() {
    assert.equal(not(isPositive).expected, "not positive number");
    assert.equal(not(isDefined).expected, "not defined");
  });
});

describe('and()', function () {
  it('should return true when all the original contracts are true', function() {
    assert.equal(and(isNumber, isPositive)(42), true);
    assert.equal(and(isNumber, not(isPositive), not(isNegative))(0), true);
  });
  it('should return false when any of the original contracts were false', function() {
    assert.equal(and(isNumber, isPositive, isNegative)(7), false);
    assert.equal(and(isNumber, isPositive, isNegative)(-7), false);
    assert.equal(and(isDefined, isString)(42), false);
    assert.equal(and(isBoolean)(42), false);
  });
  it('should return a function with an "expected" property set to something informative', function() {
    assert.equal(and(isDefined, isString).expected, "defined and string");
    assert.equal(and(isNumber, isPositive, isDefined).expected, "number and positive number and defined");
  });
});

describe('or()', function() {
  it('should return true when any of the original contracts are true', function() {
    assert.equal(or(isString, isNumber)(42), true);
    assert.equal(or(isString, isNumber, isBoolean, isDefined)({}), true);
  });
  it('should return false when all of the original contracts are false', function() {
    assert.equal(or(isString, isBoolean, isNegative)(42), false);
  });
  it('should return a function with an "expected" property set to something informative', function() {
    assert.equal(or(isDefined, isString).expected, "defined or string");
    assert.equal(or(isNumber, isPositive, isDefined).expected, "number or positive number or defined");
  });
});

describe('contract()', function() {
  it('should return true when any of the original contracts are true', function() {
    mult = contract([isNumber, isNumber],
      isNumber,
      function mult (x, y) {
        return x+y;
      });
    result = mult(4, 3);
    console.log(result);
    assert.equal(result, 7);
    });
});
