MaxLang Compiler v1.0
Maximum Finder Language - Complete Compiler Implementation

📋 Project Overview
MaxLang is a domain-specific programming language designed for finding the maximum value between integers. This project implements a complete compiler featuring lexical analysis, syntax analysis with AST generation, semantic analysis with symbol table management, and an interpreter for execution.
Project Goals

Learning: Demonstrate comprehensive understanding of compiler construction phases
Performance: Efficient lexical scanning and parsing with O(n) complexity
Research: Explore symbol table design and type checking mechanisms
Production Use: Create a functional, well-tested compiler with robust error handling

Success Criteria
✅ Complete implementation of all compiler phases
✅ Comprehensive error handling and recovery
✅ Symbol table with proper scope management
✅ Type checking and semantic validation
✅ 10+ test cases covering all scenarios
✅ Complete documentation and user guide

🎯 Language Features
Supported Features

Integer Variables: Declaration and assignment (x = 5;)
Built-in Functions: max(a, b) - returns maximum of two values
Arithmetic Operations: +, -, *, /
Print Statement: Output results (print expression;)
Comments: Single-line comments (// comment)
Expression Evaluation: Full arithmetic expression support

Example MaxLang Program
maxlang// Find maximum of two numbers
x = 5;
y = 3;
print max(x, y);

// With arithmetic expressions
a = 10 + 5;
b = 20 - 3;
print max(a, b);

// Nested max calls
result = max(max(x, y), max(a, b));
print result;

🏗️ Architecture & Implementation
Compiler Phases
Phase 1: Lexical Analysis (Scanner/Tokenizer)

Token Types: Keywords, Identifiers, Numbers, Operators, Delimiters
Features:

Line and column tracking for error reporting
Comment handling
Whitespace skipping
Invalid token detection


Output: Stream of tokens with metadata

Phase 2: Syntax Analysis (Parser)

Parser Type: Recursive Descent Parser
Grammar: LL(1) compatible
Features:

AST (Abstract Syntax Tree) generation
Error recovery mechanisms
Support for operator precedence
Expression parsing with proper associativity


Output: Complete AST representation

Phase 3: Semantic Analysis

Symbol Table: Hash-based implementation
Features:

Variable declaration tracking
Type checking (integer type system)
Scope management (global scope)
Initialization checking
Undefined variable detection


Output: Validated AST with symbol information

Phase 4: Execution (Interpreter)

Approach: Tree-walking interpreter
Features:

Direct AST evaluation
Runtime expression evaluation
Built-in function execution
Runtime error handling


Output: Program results


📖 Formal Grammar Specification
ebnf<program>       ::= <statement_list>

<statement_list>::= <statement>
                 | <statement> <statement_list>

<statement>     ::= <assignment>
                 | <print_stmt>

<assignment>    ::= <identifier> '=' <expression> ';'

<print_stmt>    ::= 'print' <expression> ';'

<expression>    ::= <additive>

<additive>      ::= <multiplicative>
                 | <additive> '+' <multiplicative>
                 | <additive> '-' <multiplicative>

<multiplicative>::= <primary>
                 | <multiplicative> '*' <primary>
                 | <multiplicative> '/' <primary>

<primary>       ::= <number>
                 | <identifier>
                 | <max_call>
                 | '(' <expression> ')'

<max_call>      ::= 'max' '(' <expression> ',' <expression> ')'

<identifier>    ::= [a-zA-Z_][a-zA-Z0-9_]*

<number>        ::= [0-9]+

🛠️ Technology Stack
Core Technologies

Language: C++17
Build System: g++/clang++ with Make/CMake support
Standard Library: STL (vector, map, string, memory)

Design Patterns

Visitor Pattern: AST traversal
Factory Pattern: Token and AST node creation
Singleton Pattern: Symbol table management

Data Structures

Hash Maps: Symbol table implementation
Trees: AST representation
Vectors: Token stream storage


🚀 Building & Running
Prerequisites

C++17 compatible compiler (g++ 7+, clang++ 5+, MSVC 2017+)
Make or CMake (optional)

Compilation
Using g++
bashg++ -std=c++17 -Wall -Wextra -O2 maxlang_compiler.cpp -o maxlang
Using clang++
bashclang++ -std=c++17 -Wall -Wextra -O2 maxlang_compiler.cpp -o maxlang
Using CMake
bashmkdir build && cd build
cmake ..
make
Running the Compiler
Run Test Suite
bash./maxlang
Interactive Mode
After test suite completion, enter interactive mode:
MaxLang> x = 10;
       > y = 20;
       > print max(x, y);
       > [Press Enter on empty line to compile]
Compile from File (Add this feature)
bash./maxlang input.max

🧪 Test Suite
Test Coverage
TEST 1: Basic Max Function
maxlangx = 5;
y = 3;
print max(x, y);
Expected Output: 5
TEST 2: Max with Arithmetic
maxlanga = 10;
b = 20;
print max(a + 5, b - 3);
Expected Output: 17
TEST 3: Nested Max Calls
maxlangx = 5;
y = 10;
z = 7;
print max(max(x, y), z);
Expected Output: 10
TEST 4: Multiple Statements
maxlangnum1 = 100;
num2 = 50;
print num1;
print num2;
print max(num1, num2);
Expected Output: 100, 50, 100
TEST 5: Complex Expressions
maxlangx = 2 * 3 + 4;
y = 10 - 2 * 2;
print max(x, y);
Expected Output: 10
TEST 6: Error Detection - Undefined Variable
maxlangprint max(x, y);
Expected: Semantic Error
TEST 7: Comments Support
maxlang// This is a comment
x = 15;
// Another comment
y = 25;
print max(x, y);
Expected Output: 25
TEST 8: Direct Numbers
maxlangprint max(42, 17);
Expected Output: 42
TEST 9: Equal Values
maxlanga = 100;
b = 100;
print max(a, b);
Expected Output: 100
TEST 10: Complex Nested
maxlangw = 5;
x = 10;
y = 15;
z = 20;
result = max(w + x, y * 2);
print result;
print max(result, z);
Expected Output: 30, 30

📊 Compiler Output Examples
Complete Compilation Process
╔════════════════════════════════════════════════════════════╗
║         MaxLang Compiler - v1.0                            ║
║         Maximum Finder Language Compiler                   ║
╚════════════════════════════════════════════════════════════╝

=== SOURCE CODE ===
============================================================
x = 5;
y = 3;
print max(x, y);
============================================================

[Phase 1] Starting Lexical Analysis...

=== LEXICAL ANALYSIS - TOKEN STREAM ===
============================================================
Token(IDENTIFIER, 'x', line=1, col=1)
Token(ASSIGN, '=', line=1, col=3)
Token(NUMBER, '5', value=5, line=1, col=5)
Token(SEMICOLON, ';', line=1, col=6)
Token(IDENTIFIER, 'y', line=2, col=1)
Token(ASSIGN, '=', line=2, col=3)
Token(NUMBER, '3', value=3, line=2, col=5)
Token(SEMICOLON, ';', line=2, col=6)
Token(PRINT, 'print', line=3, col=1)
Token(MAX, 'max', line=3, col=7)
Token(LPAREN, '(', line=3, col=10)
Token(IDENTIFIER, 'x', line=3, col=11)
Token(COMMA, ',', line=3, col=12)
Token(IDENTIFIER, 'y', line=3, col=14)
Token(RPAREN, ')', line=3, col=15)
Token(SEMICOLON, ';', line=3, col=16)
Token(EOF, '', line=3, col=17)
============================================================
✓ Lexical Analysis Complete - 18 tokens generated

[Phase 2] Starting Syntax Analysis (Parsing)...

=== ABSTRACT SYNTAX TREE (AST) ===
============================================================
ProgramNode
  Statement 1:
    AssignmentNode('x')
      Expression:
        NumberNode(5)
  Statement 2:
    AssignmentNode('y')
      Expression:
        NumberNode(3)
  Statement 3:
    PrintNode
      Expression:
        MaxCallNode
          Arg1:
            IdentifierNode('x')
          Arg2:
            IdentifierNode('y')
============================================================
✓ Syntax Analysis Complete - AST generated

[Phase 3] Starting Semantic Analysis...

=== SEMANTIC ANALYSIS ===
✓ No semantic errors found
✓ Type checking passed
✓ All variables properly declared
✓ Semantic Analysis Complete

[Phase 4] Starting Program Execution...

=== PROGRAM EXECUTION ===
============================================================
Output:
5
============================================================

=== SYMBOL TABLE ===
================================================================================
Symbol         Type      Value       Initialized    Decl Line 
--------------------------------------------------------------------------------
x              int       5           Yes            0         
y              int       3           Yes            0         
================================================================================

╔════════════════════════════════════════════════════════════╗
║  ✓ Compilation and Execution Successful!                   ║
╚════════════════════════════════════════════════════════════╝

🎓 Educational Value
Compiler Construction Concepts Demonstrated

Lexical Analysis

Finite automata implementation
Token recognition patterns
Error detection and recovery


Syntax Analysis

Recursive descent parsing
AST construction
Grammar-driven design


Semantic Analysis

Symbol table design
Type system implementation
Scope resolution


Code Generation/Interpretation

AST traversal techniques
Runtime evaluation
Memory management




🐛 Error Handling
Lexical Errors

Invalid characters detection
Malformed tokens

Syntax Errors

Missing semicolons
Unmatched parentheses
Invalid statement structure
Error recovery to continue parsing

Semantic Errors

Undefined variable usage
Uninitialized variable access
Type mismatches (future extension)

Runtime Errors

Division by zero
Stack overflow (recursive expressions)


📚 Project Structure
maxlang-compiler/
├── maxlang_compiler.cpp      # Main compiler implementation
├── README.md                  # This file
├── LANGUAGE_SPEC.md          # Complete language specification
├── GRAMMAR.ebnf              # Formal grammar
├── examples/                 # Example programs
│   ├── basic.max
│   ├── arithmetic.max
│   └── nested.max
├── tests/                    # Test suite
│   ├── test_lexer.cpp
│   ├── test_parser.cpp
│   └── test_semantic.cpp
└── docs/                     # Additional documentation
    ├── ARCHITECTURE.md
    ├── API_REFERENCE.md
    └── DESIGN_DECISIONS.md

🔮 Future Enhancements
Planned Features

 min() function
 Comparison operators (<, >, ==, !=)
 Conditional statements (if-else)
 Loop constructs (while, for)
 Function definitions
 Arrays and data structures
 String type support
 File I/O operations
 LLVM IR generation
 Optimization passes


👥 Contributors
Project Author: Muhammad Muneeb & Abdul Wasay Tabba
Course: Compiler Construction
Institution: SZABIST
Academic Year: 2024-2025

📝 License
This project is created for educational purposes as part of a Compiler Construction course.

🙏 Acknowledgments

Course Instructor: [Instructor Name]
Compiler Construction Textbook: "Compilers: Principles, Techniques, and Tools" (Dragon Book)
C++ Standard Library Documentation
Open Source Compiler Projects (LLVM, GCC)


📞 Support & Contact
For questions, issues, or contributions:

Email: [bscs2312160@szabist.pk]
GitHub: [github.com/muneebpardar/maxlang]
Documentation: See docs/ folder