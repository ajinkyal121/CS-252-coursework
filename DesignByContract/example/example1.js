var contracts = require('contracts');

var contract = contracts.contract;
var isNumber = contracts.isNumber;

var mult = contract(
  [isNumber, isNumber],
  isNumber,
  function mult (x, y) {
    return x+y;
  });

var brokenMult = contract(
  [isNumber, isNumber],
  isNumber,
  function brokenMult(x, y) {
    return undefined;
  });

console.log("First test")
console.log(mult(3, 4));
console.log();

console.log("Second test")
try {
  console.log(mult(3, "four"));
} catch (e) {
  console.log(e.message);
}
console.log();

console.log("Third test")
try {
  console.log(brokenMult(3, 4));
} catch (e) {
  console.log(e.message);
}
console.log();

console.log("Fourth test");
try {
  console.log(brokenMult(3, "four"));
} catch (e) {
  console.log(e.message);
}
console.log();

console.log("Fifth test");
function fifth() {
  console.log(brokenMult(5, "five"));
};
try {
  fifth();
} catch (e) {
  console.log(e.message);
}


