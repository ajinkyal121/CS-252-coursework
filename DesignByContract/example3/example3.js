// This version of the Account example uses more advanced JS features. In order
// to handle it, you will probably need to use JS proxies in your contracts library.
// Unlike our previous example, contracts here use properties of the object.
// Any calls to "this" in the contract refers to the object being tested.
//
// Note a slight revision in the API -- If Account.prototype has an invariant field,
// that will be enforced for all calls to Account methods with a contract.
// (We do not protect against abuse from other function calls, nor do we
// stop direct access to fields.)

var {
    contract: contract,
    any: any,
    isInteger: isInteger,
    isNumber: isNumber,
    isPositive: isPositive,
    isNegative: isNegative,
    and: and,
    not: not,
    negative: negative,
  } = require('contracts');
  
  function Account(name, balance) {
    this.name = name;
    this.balance = balance;
  }
  // Custom contract.  "This" should be the account, not the global object.
  Account.prototype.sufficientFunds = function(v) {
    return isPositive(v) && v <= this.balance;
  }
  Account.prototype.sufficientFunds.expected = "sufficent funds";
  Account.prototype.deposit = contract(
    [and(isPositive, isInteger)],
    any,
    function deposit(amt) {
      this.balance += amt;
    }
  );
  Account.prototype.withdraw = contract(
    [Account.prototype.sufficientFunds],
    any,
    function(amt) {
      this.balance -= amt;
    }
  );
  
  
  var a = new Account("Alice", 500);
  try {
    a.deposit(42);
    a.withdraw(100);
    a.withdraw(100);
    a.withdraw(100);
    console.log(a.balance);
  
    a.withdraw(100);
    a.deposit(90);
    a.withdraw(100);
    a.withdraw(100);
    console.log(a.balance);
  
    // Next statement should be a contract violation.
    a.withdraw(100);
    console.log(a.balance);
  } catch (e) {
    console.log(e.message);
  }
  
  console.log(a.balance);
  
  