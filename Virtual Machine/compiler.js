"use strict";

const fs = require('fs');

const opcodes = require('./op-codes.js').opcodes;

const MAX_BUFF_SIZE = 256;

// Constants for types
const LIST = 1;
const OP = 2;
const NUM = 3;
const BOOL = 4;
const VAR = 5;

/**
 * The Compiler class is responsible for taking a .scm
 * text file and converting it into bytecode format.
 */
class Compiler {
  /**
   * Constructor.
   */
  constructor() {
    this.buildMnemonicLookup();
    this.varMap = {};
    this.varOffset = 0;
  }

  /**
   * Tokenizes a Scheme file, stripping out any comments.
   * 
   * @param {String} contents - Scheme file, as text.
   * 
   * @returns {[String]} - Array of tokens, represented as strings.
   */
  tokenize(contents) {
    let lines = contents.trim().split('\n')
    let tokens = [];
    lines.forEach((ln) => {
      // Ensuring that parens are always surrounded
      // by spaces to simplify parsing.
      ln = ln.replaceAll("(", " ( ")
             .replaceAll(")", " ) ");

      // The comment character in Scheme is ';'
      ln = ln.replace(/;.*/, "");
      
      tokens.push(...ln.split(/\s+/).filter(s=>s.length!==0));
    });
    return tokens;
  }

  /**
   * Parses a stream of tokens, returning an array of objects
   * representing the top-level Scheme lists in the program.
   * (Note that in Scheme, a list is treated as a function call.)
   * 
   * @param {[String]} tokens - An array of tokens.
   * 
   * @returns {[Object]} - The AST, as a JS object literal.
   */
  parse(tokens) {
    // The top level AST does not have a type.
    let ast = { children: []};
    for (let i=0; i<tokens.length; i++) {
      let tok = tokens[i];
      if (tok === "(") {
        let newAst = { parent: ast, type: LIST, children: [] };
        ast.children.push(newAst);
        ast = newAst;
      } else if (tok === ")") {
        ast = ast.parent;
      } else if (tok.match(/^\d+$/)) {
        ast.children.push({ type: NUM, value: parseInt(tok) });
      } else if (tok === "#t") {
        ast.children.push({ type: BOOL, value: true });
      } else if (tok === "#f") {
        ast.children.push({ type: BOOL, value: false });
      } else if (tok.match(/^\w+$/)) {
        ast.children.push({ type: VAR, value: tok });
      } else {
        ast.children.push({ type: OP, value: tok})
      }
    }
    return ast.children;
  }

  /**
   * Prints out an AST, filtering out circular references.
   * 
   * @param {Object} ast - The AST to print.
   */
  printAST(ast) {
    console.log(`AST is ${JSON.stringify(ast, (key, value) => {
      if (key === 'parent') return value.id;
      else return value;
    })}`);
  }

  /**
   * Writes a byte to the next position in the bytecode buffer,
   * updating the offset to the position for the new write.
   * 
   * @param {Number} byte - A valid byte.
   */
  writeByte(byte) {
    this.offset = this.bytecode.writeUInt8(byte, this.offset);
  }

  /**
   * Looks up the opcode by its mnemonic and writes it to
   * the bytecode buffer.
   * 
   * @param {String} mnemonic - The mnemonic for the opcode.
   */
  writeOp(mnemonic) {
    let opcode = this.lookupTable[mnemonic];
    if (opcode === undefined) {
      throw new Error(`The mnemonic ${mnemonic} is not defined.`);
    }
    this.writeByte(this.lookupTable[mnemonic]);
  }

  /**
   * Converts AST into binary bytecode.
   *
   * @param {Object} ast - abstract syntax tree of program.
   */
  writeBytecode(ast) {
    if (ast.type === NUM) {
      // Numbers are just pushed on to the stack.
      this.writeOp('PUSH1');
      this.writeByte(ast.value);
      return;
    } else if (ast.type === BOOL) {
      //
      // ***YOUR CODE HERE***
      //
      // Booleans will be stored as either 1 for true, or as a 0 for false.

      return;
    } else if (ast.type === VAR) {
      //
      // ***YOUR CODE HERE***
      //
      // We look up the offset for a variable and push the offset
      // value on to the stack.  The 'MLOAD' operation will
      // retrieve the value stored at that position in the memory.

      this.writeOp('PUSH1');
      this.writeByte(this.varMap[ast.value]);
      this.writeOp('MLOAD');

      return;
    }

    // If we made it hear, we have a list.
    // The first argument is the name of the 'function'
    // that we will be invoking.
    let first = ast.children[0];

    // Almost all functions need some special handling for the
    // first argument.  Some functions will need the additional
    // arguments stored in 'rest'.
    let second = ast.children[1];
    let rest = ast.children.slice(2);

    switch (first.value) {
      case "println":
        this.writeBytecode(second);
        this.writeOp('PRINT');
        break;

      case "define":
        //
        // ***YOUR CODE HERE***
        //
        // The define function lets us store variables.
        //
        // The variable name is stored in 'second.value'.
        // Update the 'this.varMap' array to store the current
        // value of 'this.varOffset'.
        //
        // The VM will need to push the value on to the stack,
        // push 'this.varOffset' on to the stack, and then
        // invoke 'MSTORE'.
        //
        // Increment this.varOffset so that it points to the next
        // position in memory.
        
        this.varMap[second.value] = this.varOffset;

        this.writeBytecode(rest[0]);
        this.writeOp('PUSH1');
        this.writeByte(this.varOffset);
        this.writeOp('MSTORE');
        
        this.varOffset = this.varOffset + 1;

        break;

      case "if":
        //
        // ***YOUR CODE HERE***
        //
        // EXTRA CREDIT!
        // Add support for if expressions.
        // The cond.scm file gives you some good examples.
        break;

      case "+":

        this.writeBytecode(second);
        rest.forEach((x) => {
          this.writeBytecode(x);
          this.writeOp('ADD');
        });
        break;

      case "*":

        this.writeBytecode(second);
        this.writeBytecode(rest[0]);    // multiply only two argument and ignmore the rest
        this.writeOp('MUL');
        // multiply arbitrary nyumber of argument
        // rest.forEach((x) => {
        //     this.writeBytecode(x);
        //     this.writeOp('MUL');
        //   });
        break;

      case "-":

        this.writeBytecode(second);
        rest.forEach((x) => {
          this.writeBytecode(x);
          this.writeOp('SWAP1');
          this.writeOp('SUB');
        });
        break;

      default:
        throw new Error(`Unexpected head: '${first.value}'`);
    }
  }

  /**
   * Builds up a mapping of opcode mnemonics to the corresponding
   * hexadecimal values.
   */
  buildMnemonicLookup() {
    this.lookupTable = {};
    Object.keys(opcodes).forEach((opcode) => {
      let inst = opcodes[opcode];
      this.lookupTable[inst.mnemonic] = opcode;
    });
  }

  /**
   * This method takes a scheme file, tokenizes and parses it,
   * and finally compiles it to binary bytecode.
   * 
   * @param {String} fileName - The name of the scheme file.
   * 
   * @returns {String} - The name of the bytecode file.
   */
  compileScheme(fileName) {
    if (!fileName.toLowerCase().endsWith('.scm')) {
      throw new Error(`${fileName} does not end with a .scm extension.`);
    }

    fs.readFileSync(fileName);
    let contents = fs.readFileSync(fileName, 'utf8');

    let tokens = this.tokenize(contents);
    let asts = this.parse(tokens);

    // The bytecode size cannot be larger than MAX_BUFF_SIZE.
    this.bytecode = Buffer.alloc(MAX_BUFF_SIZE);
    // The offset tracks the current position in the bytecode buffer.
    this.offset = 0;

    asts.forEach((ast) => {
      this.writeBytecode(ast);
    });

    // The output file will have the same name as the input file,
    // except that '.scm' will be replaced with '.byco'.
    let outputFile = fileName.replace(/.scm\b/i, ".byco");
    fs.writeFileSync(outputFile, this.bytecode.slice(0, this.offset, 'hex'));

    return outputFile;
  }
}

// Handling command line arguments.
if (process.argv0 === 'node') {
  process.argv.shift();
}
if (process.argv.length !== 2) {
  console.log("compiler.js <scheme file>");
  process.exit(1);
}

let cmplr = new Compiler();
let scmFile = process.argv[1];

console.log(`Compiling ${scmFile}...`);
let bytecodeFile = cmplr.compileScheme(scmFile);

console.log(`Bytecode written to ${bytecodeFile}.`);