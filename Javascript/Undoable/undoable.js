"use strict";

// Input: any object.
//
// Returns: An object with a 'revert' method that reverts
//    any changes to the specified field.
function makeUndoable(o) {
  let oldVals = {};
  return new Proxy(o, {
    get: (target, name) => {
      // Adding a 'revert' method on undoable objects,
      // which is not part of the original object.
      if (name === 'revert') {
        return function(p) {
          target[p] = oldVals[p];
        }
        // If any property other than 'revert' is requested,
        // return the value from the underlying object.
      } else return target[name];
    },
    set: (target, name, val) => {
      oldVals[name] = target[name];
      o[name] = val;
      // Returns true to indicate that assignment was successful.
      return true;
    },
    deleteProperty: (target, name) => {
      oldVals[name] = target[name];
      return delete target[name];
    },
  });
}

let emp = { fname: "Joe", lname: "Smith", id: 1234, salary: 100 };
emp = makeUndoable(emp);

emp.fname = "Joey";
console.log(`Joe's name accidentally changed to ${emp.fname}`);

emp.revert('fname');
console.log(`Joe's name changed back to ${emp.fname}`);

delete emp.lname;
console.log(`${emp.fname}'s last name is now ${emp.lname}`);
emp.revert('lname');
console.log(`${emp.fname}'s last name is now ${emp.lname}`);
