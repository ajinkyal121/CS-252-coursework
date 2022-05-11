// This example is using a standard JS design pattern to hide variables.
// The balance variable is captured as a closure.  Critically, it is
// **not** a field of Account objects.  Instead, getter/setter fields
// are used to provide access to that variable.
//
// As a side-effect, some challenges of writing contracts for objects
// are nicely side-stepped.  Note that this example includes a contract
// specifying details about "private" data.


// Note: An 'import' feature is available for ES6,
// but it does not yet seem to be implemented for Node.js.
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
  
  // Note that balance is effectively private.
  function Account(name, balance) {
    this.name = name;
  
    // Custom contract
    function lessThanBalance(v) {
      return v <= balance;
    }
    // Method to deposit money
    this.deposit = contract(
      [and(isPositive, isInteger)],
      any,
      function (amt) { balance += amt; });
    // Method to withdraw money
    this.withdraw = contract(
      [and(isPositive,isNumber,lessThanBalance)],
      any,
      function(amt) {
        balance -= amt;
      });
    // Method to show the current balance
    this.getBalance = function () {
      return balance;
    }
  }
  
  var a = new Account("Alice", 500);
  
  console.log("Trying to deposit a negative amount");
  try {
    a.deposit(-42);
  } catch (e) {
    console.log(e.message);
  }
  console.log();
  
  console.log("Trying to withdraw a negative amount");
  try {
    a.withdraw(-42);
  } catch (e) {
    console.log(e.message);
  }
  console.log();
  
  try {
    a.deposit(42);
    a.withdraw(100);
    a.withdraw(100);
    a.withdraw(100);
    console.log(a.getBalance());
  
    a.withdraw(100);
    a.deposit(90);
    a.withdraw(100);
    a.withdraw(100);
    console.log(a.getBalance());
  
    // The next statement should be a contract violation.
    a.withdraw(100);
    console.log(a.getBalance());
  } catch (e) {
    console.log(e.message);
  }
  
  console.log("Final balance: " + a.getBalance());