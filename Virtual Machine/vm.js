'use strict';

const fs = require('fs');
const opcodes = require('./op-codes.js').opcodes;

/**
 * The VM is responsible for executing the bytecode format.
 */
class VirtualMachine {

  /**
   * Initializes the virtual machine with the specified amount of
   * gas.  The stack and memory are both initially empty.
   * 
   * @param {Number} gas - Amount of gas the VM begins with.
   */
  constructor() {
    this.stack = [];
    this.memory = [];
  }

  /** 
   * Loads a bytecode file and returns an array of strings,
   * which are the commands within the file.
   */
  static loadBytecode(bytecodeFile) {
    let contents = fs.readFileSync(bytecodeFile);
    return Buffer.from(contents);
  }

  /**
   * Evaluates the specified file.
   */
  evaluate(bytecodeFile) {
    this.bytecode = this.constructor.loadBytecode(bytecodeFile);

    // Initializing the program counter to keep track of our
    // place within the program.
    this.pc = 0;

    while (this.pc < this.bytecode.length) {
      let opcode = this.bytecode.readUInt8(this.pc);
      //console.log(`Evaluating ${opcode.toString(16)}`);
      let operation = opcodes[opcode];
      if (operation === undefined) {
        throw new Error(`Unable to find instruction for ${opcode.toString(16)}`);
      }
      operation.evaluate(this);

      this.pc++;
    }
  }
}

// Handling command line arguments.
if (process.argv0 === 'node') {
  process.argv.shift();
}
if (process.argv.length !== 2) {
  console.log("vm.js <bytecode file>");
  process.exit(1);
}

let bytecodeFile = process.argv[1];

console.log(`Executing ${bytecodeFile}...`);
let vm = new VirtualMachine();
vm.evaluate(bytecodeFile);