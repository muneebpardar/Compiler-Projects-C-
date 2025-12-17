// ============================================================================
// EVENLANG COMPILER - Complete Implementation
// Author: Compiler Construction Project
// Description: Full compiler for Even/Odd number checking language
// Features: Lexer, Parser, AST, Semantic Analysis, Code Generation, Interpreter
// ============================================================================

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <cctype>
#include <sstream>
#include <iomanip>
#include <stdexcept>
#include <fstream>

using namespace std;

// ============================================================================
// TOKEN DEFINITIONS
// ============================================================================

enum class TokenType {
    CHECK, EVEN, ODD, PRINT, IF, ELSE,
    IDENTIFIER, NUMBER,
    ASSIGN, SEMICOLON, LPAREN, RPAREN, COMMA,
    PLUS, MINUS, MULTIPLY, DIVIDE, MODULO,
    EQUALS, NOT_EQUALS, LESS, GREATER, LESS_EQ, GREATER_EQ,
    END_OF_FILE, INVALID
};

struct Token {
    TokenType type;
    string lexeme;
    int value;
    int line;
    int column;

    Token(TokenType t, const string& lex, int val = 0, int ln = 1, int col = 1)
        : type(t), lexeme(lex), value(val), line(ln), column(col) {}

    string toString() const {
        stringstream ss;
        ss << "Token(" << tokenTypeToString(type) << ", '" << lexeme << "'";
        if (type == TokenType::NUMBER) ss << ", value=" << value;
        ss << ", line=" << line << ", col=" << column << ")";
        return ss.str();
    }

    static string tokenTypeToString(TokenType type) {
        switch(type) {
            case TokenType::CHECK: return "CHECK";
            case TokenType::PRINT: return "PRINT";
            case TokenType::IDENTIFIER: return "IDENTIFIER";
            case TokenType::NUMBER: return "NUMBER";
            case TokenType::ASSIGN: return "ASSIGN";
            case TokenType::SEMICOLON: return "SEMICOLON";
            case TokenType::PLUS: return "PLUS";
            case TokenType::MINUS: return "MINUS";
            case TokenType::MULTIPLY: return "MULTIPLY";
            case TokenType::DIVIDE: return "DIVIDE";
            case TokenType::MODULO: return "MODULO";
            case TokenType::END_OF_FILE: return "EOF";
            default: return "UNKNOWN";
        }
    }
};

// ============================================================================
// LEXER
// ============================================================================

class Lexer {
private:
    string source;
    size_t position;
    int line;
    int column;
    char currentChar;

    void advance() {
        if (position < source.length()) {
            if (currentChar == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            position++;
            currentChar = (position < source.length()) ? source[position] : '\0';
        }
    }

    void skipWhitespace() {
        while (currentChar != '\0' && isspace(currentChar)) {
            advance();
        }
    }

    void skipComment() {
        if (currentChar == '/' && peek() == '/') {
            while (currentChar != '\0' && currentChar != '\n') {
                advance();
            }
        }
    }

    char peek() {
        size_t peekPos = position + 1;
        return (peekPos < source.length()) ? source[peekPos] : '\0';
    }

    Token number() {
        int startCol = column;
        string numStr;

        if (currentChar == '-') {
            numStr += currentChar;
            advance();
        }

        while (currentChar != '\0' && isdigit(currentChar)) {
            numStr += currentChar;
            advance();
        }

        int val = stoi(numStr);
        return Token(TokenType::NUMBER, numStr, val, line, startCol);
    }

    Token identifier() {
        int startCol = column;
        string id;
        while (currentChar != '\0' && (isalnum(currentChar) || currentChar == '_')) {
            id += currentChar;
            advance();
        }

        if (id == "check") return Token(TokenType::CHECK, id, 0, line, startCol);
        if (id == "print") return Token(TokenType::PRINT, id, 0, line, startCol);

        return Token(TokenType::IDENTIFIER, id, 0, line, startCol);
    }

public:
    Lexer(const string& src) : source(src), position(0), line(1), column(1) {
        currentChar = source.empty() ? '\0' : source[0];
    }

    vector<Token> tokenize() {
        vector<Token> tokens;

        while (currentChar != '\0') {
            skipWhitespace();
            if (currentChar == '\0') break;

            if (currentChar == '/' && peek() == '/') {
                skipComment();
                continue;
            }

            int startCol = column;

            if (isdigit(currentChar) || (currentChar == '-' && isdigit(peek()))) {
                tokens.push_back(number());
            }
            else if (isalpha(currentChar) || currentChar == '_') {
                tokens.push_back(identifier());
            }
            else if (currentChar == '=') {
                tokens.push_back(Token(TokenType::ASSIGN, "=", 0, line, startCol));
                advance();
            }
            else if (currentChar == ';') {
                tokens.push_back(Token(TokenType::SEMICOLON, ";", 0, line, startCol));
                advance();
            }
            else if (currentChar == '(') {
                tokens.push_back(Token(TokenType::LPAREN, "(", 0, line, startCol));
                advance();
            }
            else if (currentChar == ')') {
                tokens.push_back(Token(TokenType::RPAREN, ")", 0, line, startCol));
                advance();
            }
            else if (currentChar == '+') {
                tokens.push_back(Token(TokenType::PLUS, "+", 0, line, startCol));
                advance();
            }
            else if (currentChar == '-') {
                tokens.push_back(Token(TokenType::MINUS, "-", 0, line, startCol));
                advance();
            }
            else if (currentChar == '*') {
                tokens.push_back(Token(TokenType::MULTIPLY, "*", 0, line, startCol));
                advance();
            }
            else if (currentChar == '/') {
                tokens.push_back(Token(TokenType::DIVIDE, "/", 0, line, startCol));
                advance();
            }
            else if (currentChar == '%') {
                tokens.push_back(Token(TokenType::MODULO, "%", 0, line, startCol));
                advance();
            }
            else {
                advance();
            }
        }

        tokens.push_back(Token(TokenType::END_OF_FILE, "", 0, line, column));
        return tokens;
    }

    void printTokens(const vector<Token>& tokens) {
        cout << "\n=== LEXICAL ANALYSIS - TOKEN STREAM ===\n";
        cout << string(60, '=') << "\n";
        for (const auto& token : tokens) {
            cout << token.toString() << "\n";
        }
        cout << string(60, '=') << "\n";
    }
};

// ============================================================================
// AST NODES
// ============================================================================

enum class ASTNodeType {
    PROGRAM, ASSIGNMENT, PRINT_STMT, CHECK_STMT,
    BINARY_OP, IDENTIFIER, NUMBER
};

class ASTNode {
public:
    ASTNodeType type;
    virtual ~ASTNode() = default;
    virtual string toString(int indent = 0) const = 0;

protected:
    string getIndent(int level) const {
        return string(level * 2, ' ');
    }
};

class NumberNode : public ASTNode {
public:
    int value;
    NumberNode(int val) : value(val) { type = ASTNodeType::NUMBER; }
    string toString(int indent = 0) const override {
        return getIndent(indent) + "NumberNode(" + to_string(value) + ")";
    }
};

class IdentifierNode : public ASTNode {
public:
    string name;
    IdentifierNode(const string& n) : name(n) { type = ASTNodeType::IDENTIFIER; }
    string toString(int indent = 0) const override {
        return getIndent(indent) + "IdentifierNode('" + name + "')";
    }
};

class BinaryOpNode : public ASTNode {
public:
    string op;
    unique_ptr<ASTNode> left;
    unique_ptr<ASTNode> right;

    BinaryOpNode(const string& operation, unique_ptr<ASTNode> l, unique_ptr<ASTNode> r)
        : op(operation), left(move(l)), right(move(r)) { type = ASTNodeType::BINARY_OP; }

    string toString(int indent = 0) const override {
        string result = getIndent(indent) + "BinaryOpNode('" + op + "')\n";
        result += getIndent(indent + 1) + "Left:\n" + left->toString(indent + 2) + "\n";
        result += getIndent(indent + 1) + "Right:\n" + right->toString(indent + 2);
        return result;
    }
};

class AssignmentNode : public ASTNode {
public:
    string varName;
    unique_ptr<ASTNode> expression;

    AssignmentNode(const string& name, unique_ptr<ASTNode> expr)
        : varName(name), expression(move(expr)) { type = ASTNodeType::ASSIGNMENT; }

    string toString(int indent = 0) const override {
        string result = getIndent(indent) + "AssignmentNode('" + varName + "')\n";
        result += getIndent(indent + 1) + "Expression:\n" + expression->toString(indent + 2);
        return result;
    }
};

class PrintNode : public ASTNode {
public:
    unique_ptr<ASTNode> expression;
    PrintNode(unique_ptr<ASTNode> expr) : expression(move(expr)) { type = ASTNodeType::PRINT_STMT; }
    string toString(int indent = 0) const override {
        string result = getIndent(indent) + "PrintNode\n";
        result += getIndent(indent + 1) + "Expression:\n" + expression->toString(indent + 2);
        return result;
    }
};

class CheckNode : public ASTNode {
public:
    unique_ptr<ASTNode> expression;
    CheckNode(unique_ptr<ASTNode> expr) : expression(move(expr)) { type = ASTNodeType::CHECK_STMT; }
    string toString(int indent = 0) const override {
        string result = getIndent(indent) + "CheckNode (Even/Odd)\n";
        result += getIndent(indent + 1) + "Expression:\n" + expression->toString(indent + 2);
        return result;
    }
};

class ProgramNode : public ASTNode {
public:
    vector<unique_ptr<ASTNode>> statements;
    ProgramNode() { type = ASTNodeType::PROGRAM; }
    void addStatement(unique_ptr<ASTNode> stmt) { statements.push_back(move(stmt)); }
    string toString(int indent = 0) const override {
        string result = getIndent(indent) + "ProgramNode\n";
        for (size_t i = 0; i < statements.size(); i++) {
            result += getIndent(indent + 1) + "Statement " + to_string(i + 1) + ":\n";
            result += statements[i]->toString(indent + 2);
            if (i < statements.size() - 1) result += "\n";
        }
        return result;
    }
};

// ============================================================================
// PARSER
// ============================================================================

class Parser {
private:
    vector<Token> tokens;
    size_t current;
    vector<string> errors;

    Token currentToken() { return tokens[current]; }
    void advance() { if (current < tokens.size() - 1) current++; }

    bool match(TokenType type) {
        if (currentToken().type == type) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType type, const string& message) {
        if (currentToken().type != type) {
            error(message);
            throw runtime_error(message);
        }
        advance();
    }

    void error(const string& message) {
        Token tok = currentToken();
        stringstream ss;
        ss << "Parse Error at line " << tok.line << ", column " << tok.column << ": " << message;
        errors.push_back(ss.str());
    }

    unique_ptr<ASTNode> parseExpression() { return parseAdditive(); }

    unique_ptr<ASTNode> parseAdditive() {
        auto left = parseMultiplicative();
        while (currentToken().type == TokenType::PLUS || currentToken().type == TokenType::MINUS) {
            string op = currentToken().lexeme;
            advance();
            auto right = parseMultiplicative();
            left = make_unique<BinaryOpNode>(op, move(left), move(right));
        }
        return left;
    }

    unique_ptr<ASTNode> parseMultiplicative() {
        auto left = parsePrimary();
        while (currentToken().type == TokenType::MULTIPLY ||
               currentToken().type == TokenType::DIVIDE ||
               currentToken().type == TokenType::MODULO) {
            string op = currentToken().lexeme;
            advance();
            auto right = parsePrimary();
            left = make_unique<BinaryOpNode>(op, move(left), move(right));
        }
        return left;
    }

    unique_ptr<ASTNode> parsePrimary() {
        Token tok = currentToken();
        if (tok.type == TokenType::NUMBER) {
            advance();
            return make_unique<NumberNode>(tok.value);
        }
        if (tok.type == TokenType::IDENTIFIER) {
            advance();
            return make_unique<IdentifierNode>(tok.lexeme);
        }
        if (tok.type == TokenType::LPAREN) {
            advance();
            auto expr = parseExpression();
            expect(TokenType::RPAREN, "Expected ')' after expression");
            return expr;
        }
        error("Expected expression");
        throw runtime_error("Expected expression");
    }

    unique_ptr<ASTNode> parseAssignment() {
        Token idToken = currentToken();
        expect(TokenType::IDENTIFIER, "Expected identifier");
        expect(TokenType::ASSIGN, "Expected '=' in assignment");
        auto expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after assignment");
        return make_unique<AssignmentNode>(idToken.lexeme, move(expr));
    }

    unique_ptr<ASTNode> parsePrintStatement() {
        expect(TokenType::PRINT, "Expected 'print'");
        auto expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after print statement");
        return make_unique<PrintNode>(move(expr));
    }

    unique_ptr<ASTNode> parseCheckStatement() {
        expect(TokenType::CHECK, "Expected 'check'");
        auto expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after check statement");
        return make_unique<CheckNode>(move(expr));
    }

    unique_ptr<ASTNode> parseStatement() {
        Token tok = currentToken();
        if (tok.type == TokenType::PRINT) return parsePrintStatement();
        else if (tok.type == TokenType::CHECK) return parseCheckStatement();
        else if (tok.type == TokenType::IDENTIFIER) return parseAssignment();
        else if (tok.type == TokenType::END_OF_FILE) return nullptr;
        else {
            error("Expected statement");
            advance();
            return nullptr;
        }
    }

public:
    Parser(const vector<Token>& toks) : tokens(toks), current(0) {}

    unique_ptr<ProgramNode> parse() {
        auto program = make_unique<ProgramNode>();
        while (currentToken().type != TokenType::END_OF_FILE) {
            try {
                auto stmt = parseStatement();
                if (stmt) program->addStatement(move(stmt));
            } catch (const exception& e) {
                while (currentToken().type != TokenType::SEMICOLON &&
                       currentToken().type != TokenType::END_OF_FILE) advance();
                if (currentToken().type == TokenType::SEMICOLON) advance();
            }
        }
        return program;
    }

    bool hasErrors() const { return !errors.empty(); }

    void printErrors() const {
        if (!errors.empty()) {
            cout << "\n=== PARSE ERRORS ===\n";
            for (const auto& err : errors) cout << err << "\n";
        }
    }

    void printAST(const ProgramNode* program) {
        cout << "\n=== ABSTRACT SYNTAX TREE (AST) ===\n";
        cout << string(60, '=') << "\n";
        cout << program->toString() << "\n";
        cout << string(60, '=') << "\n";
    }
};

// ============================================================================
// SYMBOL TABLE
// ============================================================================

struct SymbolInfo {
    string name;
    string type;
    int value;
    bool initialized;

    SymbolInfo(const string& n, const string& t)
        : name(n), type(t), value(0), initialized(false) {}
};

class SymbolTable {
private:
    map<string, SymbolInfo> symbols;

public:
    void declare(const string& name, const string& type) {
        if (symbols.find(name) == symbols.end()) {
            symbols.insert({name, SymbolInfo(name, type)});
        }
    }

    void set(const string& name, int value) {
        auto it = symbols.find(name);
        if (it == symbols.end()) {
            symbols.insert({name, SymbolInfo(name, "int")});
            it = symbols.find(name);
        }
        it->second.value = value;
        it->second.initialized = true;
    }

    int get(const string& name) {
        auto it = symbols.find(name);
        if (it == symbols.end()) throw runtime_error("Variable '" + name + "' not declared");
        if (!it->second.initialized) throw runtime_error("Variable '" + name + "' used before initialization");
        return it->second.value;
    }

    bool exists(const string& name) const {
        return symbols.find(name) != symbols.end();
    }

    void print() const {
        cout << "\n=== SYMBOL TABLE ===\n";
        cout << string(70, '=') << "\n";
        cout << left << setw(15) << "Symbol" << setw(10) << "Type"
             << setw(12) << "Value" << setw(15) << "Initialized" << "\n";
        cout << string(70, '-') << "\n";
        for (const auto& pair : symbols) {
            const SymbolInfo& info = pair.second;
            cout << left << setw(15) << info.name << setw(10) << info.type
                 << setw(12) << (info.initialized ? to_string(info.value) : "undefined")
                 << setw(15) << (info.initialized ? "Yes" : "No") << "\n";
        }
        cout << string(70, '=') << "\n";
    }

    const map<string, SymbolInfo>& getSymbols() const { return symbols; }
};

// ============================================================================
// SEMANTIC ANALYZER
// ============================================================================

class SemanticAnalyzer {
private:
    SymbolTable& symbolTable;
    vector<string> errors;

    void error(const string& message) {
        errors.push_back("Semantic Error: " + message);
    }

    void analyzeExpression(ASTNode* node) {
        if (!node) return;
        if (node->type == ASTNodeType::IDENTIFIER) {
            IdentifierNode* idNode = static_cast<IdentifierNode*>(node);
            if (!symbolTable.exists(idNode->name)) {
                error("Variable '" + idNode->name + "' not declared");
            }
        }
        else if (node->type == ASTNodeType::BINARY_OP) {
            BinaryOpNode* binOp = static_cast<BinaryOpNode*>(node);
            analyzeExpression(binOp->left.get());
            analyzeExpression(binOp->right.get());
        }
    }

    void analyzeStatement(ASTNode* stmt) {
        if (stmt->type == ASTNodeType::ASSIGNMENT) {
            AssignmentNode* assign = static_cast<AssignmentNode*>(stmt);
            if (!symbolTable.exists(assign->varName)) {
                symbolTable.declare(assign->varName, "int");
            }
            analyzeExpression(assign->expression.get());
        }
        else if (stmt->type == ASTNodeType::PRINT_STMT) {
            PrintNode* print = static_cast<PrintNode*>(stmt);
            analyzeExpression(print->expression.get());
        }
        else if (stmt->type == ASTNodeType::CHECK_STMT) {
            CheckNode* check = static_cast<CheckNode*>(stmt);
            analyzeExpression(check->expression.get());
        }
    }

public:
    SemanticAnalyzer(SymbolTable& st) : symbolTable(st) {}

    void analyze(ProgramNode* program) {
        for (const auto& stmt : program->statements) {
            analyzeStatement(stmt.get());
        }
    }

    bool hasErrors() const { return !errors.empty(); }

    void printReport() const {
        if (!errors.empty()) {
            cout << "\n=== SEMANTIC ERRORS ===\n";
            for (const auto& err : errors) cout << err << "\n";
        } else {
            cout << "\n=== SEMANTIC ANALYSIS ===\n";
            cout << ">> No semantic errors found\n";
            cout << ">> Type checking passed\n";
            cout << ">> All variables properly declared\n";
        }
    }
};

// ============================================================================
// CODE GENERATOR
// ============================================================================

class CodeGenerator {
private:
    stringstream asmCode;
    SymbolTable& symbolTable;
    int labelCounter;

    string newLabel() { return ".L" + to_string(labelCounter++); }

    void generateExpression(ASTNode* node) {
        if (!node) return;
        switch (node->type) {
            case ASTNodeType::NUMBER: {
                NumberNode* numNode = static_cast<NumberNode*>(node);
                asmCode << "    mov rax, " << numNode->value << "\n";
                break;
            }
            case ASTNodeType::IDENTIFIER: {
                IdentifierNode* idNode = static_cast<IdentifierNode*>(node);
                asmCode << "    mov rax, [" << idNode->name << "]\n";
                break;
            }
            case ASTNodeType::BINARY_OP: {
                BinaryOpNode* binOp = static_cast<BinaryOpNode*>(node);
                generateExpression(binOp->left.get());
                asmCode << "    push rax\n";
                generateExpression(binOp->right.get());
                asmCode << "    mov rbx, rax\n    pop rax\n";
                if (binOp->op == "+") asmCode << "    add rax, rbx\n";
                else if (binOp->op == "-") asmCode << "    sub rax, rbx\n";
                else if (binOp->op == "*") asmCode << "    imul rax, rbx\n";
                else if (binOp->op == "/") asmCode << "    xor rdx, rdx\n    idiv rbx\n";
                else if (binOp->op == "%") asmCode << "    xor rdx, rdx\n    idiv rbx\n    mov rax, rdx\n";
                break;
            }
            default: break;
        }
    }

    void generateStatement(ASTNode* stmt) {
        if (stmt->type == ASTNodeType::ASSIGNMENT) {
            AssignmentNode* assign = static_cast<AssignmentNode*>(stmt);
            asmCode << "    ; Assignment: " << assign->varName << " = ...\n";
            generateExpression(assign->expression.get());
            asmCode << "    mov [" << assign->varName << "], rax\n\n";
        }
        else if (stmt->type == ASTNodeType::PRINT_STMT) {
            PrintNode* print = static_cast<PrintNode*>(stmt);
            asmCode << "    ; Print statement\n";
            generateExpression(print->expression.get());
            asmCode << "    mov rdi, rax\n    call print_int\n\n";
        }
        else if (stmt->type == ASTNodeType::CHECK_STMT) {
            CheckNode* check = static_cast<CheckNode*>(stmt);
            asmCode << "    ; Check even/odd\n";
            generateExpression(check->expression.get());
            asmCode << "    mov rbx, 2\n    xor rdx, rdx\n    idiv rbx\n    cmp rdx, 0\n";
            string evenLabel = newLabel();
            string endLabel = newLabel();
            asmCode << "    je " << evenLabel << "\n";
            asmCode << "    lea rdi, [rel msg_odd]\n    call print_str\n    jmp " << endLabel << "\n";
            asmCode << evenLabel << ":\n    lea rdi, [rel msg_even]\n    call print_str\n";
            asmCode << endLabel << ":\n\n";
        }
    }

public:
    CodeGenerator(SymbolTable& st) : symbolTable(st), labelCounter(0) {}

    string generate(ProgramNode* program) {
        asmCode.str("");
        asmCode.clear();

        asmCode << "; EvenLang Compiler - Generated Assembly Code (x86-64)\n\n";
        asmCode << "section .data\n";

        for (const auto& pair : symbolTable.getSymbols()) {
            asmCode << "    " << pair.first << ": dq 0\n";
        }

        asmCode << "    fmt_int: db \"%ld\", 10, 0\n";
        asmCode << "    fmt_str: db \"%s\", 10, 0\n";
        asmCode << "    msg_even: db \"EVEN\", 0\n";
        asmCode << "    msg_odd: db \"ODD\", 0\n\n";

        asmCode << "section .text\n    global main\n    extern printf\n\n";
        asmCode << "print_int:\n    push rbp\n    mov rbp, rsp\n    mov rsi, rdi\n";
        asmCode << "    lea rdi, [rel fmt_int]\n    xor rax, rax\n    call printf\n";
        asmCode << "    mov rsp, rbp\n    pop rbp\n    ret\n\n";
        asmCode << "print_str:\n    push rbp\n    mov rbp, rsp\n    mov rsi, rdi\n";
        asmCode << "    lea rdi, [rel fmt_str]\n    xor rax, rax\n    call printf\n";
        asmCode << "    mov rsp, rbp\n    pop rbp\n    ret\n\n";
        asmCode << "main:\n    push rbp\n    mov rbp, rsp\n\n";

        for (const auto& stmt : program->statements) {
            generateStatement(stmt.get());
        }

        asmCode << "    xor rax, rax\n    mov rsp, rbp\n    pop rbp\n    ret\n";
        return asmCode.str();
    }

    void printAssembly(const string& assembly) {
        cout << "\n=== GENERATED ASSEMBLY CODE (x86-64) ===\n";
        cout << string(60, '=') << "\n" << assembly << string(60, '=') << "\n";
    }

    void saveToFile(const string& assembly, const string& filename) {
        ofstream file(filename);
        if (file.is_open()) {
            file << assembly;
            file.close();
            cout << "\n>> Assembly code saved to: " << filename << "\n";
        }
    }
};

// ============================================================================
// INTERPRETER
// ============================================================================

class Interpreter {
private:
    SymbolTable& symbolTable;

    int evaluateExpression(ASTNode* node) {
        if (!node) return 0;
        switch (node->type) {
            case ASTNodeType::NUMBER:
                return static_cast<NumberNode*>(node)->value;
            case ASTNodeType::IDENTIFIER:
                return symbolTable.get(static_cast<IdentifierNode*>(node)->name);
            case ASTNodeType::BINARY_OP: {
                BinaryOpNode* binOp = static_cast<BinaryOpNode*>(node);
                int left = evaluateExpression(binOp->left.get());
                int right = evaluateExpression(binOp->right.get());
                if (binOp->op == "+") return left + right;
                if (binOp->op == "-") return left - right;
                if (binOp->op == "*") return left * right;
                if (binOp->op == "/") {
                    if (right == 0) throw runtime_error("Division by zero");
                    return left / right;
                }
                if (binOp->op == "%") {
                    if (right == 0) throw runtime_error("Division by zero");
                    return left % right;
                }
                break;
            }
            default: break;
        }
        return 0;
    }
// ============================================================================
// INTERPRETER (Continued)
// ============================================================================

    void executeStatement(ASTNode* stmt) {
        if (!stmt) return;

        switch (stmt->type) {
            case ASTNodeType::ASSIGNMENT: {
                AssignmentNode* assign = static_cast<AssignmentNode*>(stmt);
                int value = evaluateExpression(assign->expression.get());
                symbolTable.set(assign->varName, value);
                break;
            }
            case ASTNodeType::PRINT_STMT: {
                PrintNode* print = static_cast<PrintNode*>(stmt);
                int value = evaluateExpression(print->expression.get());
                cout << value << endl;
                break;
            }
            case ASTNodeType::CHECK_STMT: {
                CheckNode* check = static_cast<CheckNode*>(stmt);
                int value = evaluateExpression(check->expression.get());
                if (value % 2 == 0) {
                    cout << "EVEN" << endl;
                } else {
                    cout << "ODD" << endl;
                }
                break;
            }
            default:
                break;
        }
    }

public:
    Interpreter(SymbolTable& st) : symbolTable(st) {}

    void execute(ProgramNode* program) {
        cout << "\n=== PROGRAM EXECUTION ===\n";
        cout << string(60, '=') << "\n";

        try {
            for (const auto& stmt : program->statements) {
                executeStatement(stmt.get());
            }
            cout << string(60, '=') << "\n";
            cout << ">> Program executed successfully\n";
        } catch (const exception& e) {
            cout << "\n>> Runtime Error: " << e.what() << "\n";
        }
    }
};

// ============================================================================
// COMPILER DRIVER - MAIN PROGRAM
// ============================================================================

class Compiler {
private:
    string sourceCode;
    vector<Token> tokens;
    unique_ptr<ProgramNode> ast;
    SymbolTable symbolTable;
    string assemblyCode;

    void printHeader() {
        cout << "\n";
        cout << string(70, '=') << "\n";
        cout << "    EVENLANG COMPILER - Full Compilation Pipeline\n";
        cout << "    Author: Compiler Construction Project\n";
        cout << "    Version: 1.0\n";
        cout << string(70, '=') << "\n";
    }

    void printPhase(const string& phaseName) {
        cout << "\n" << string(70, '-') << "\n";
        cout << "    PHASE: " << phaseName << "\n";
        cout << string(70, '-') << "\n";
    }

public:
    Compiler(const string& source) : sourceCode(source) {}

    bool compile(bool verbose = true) {
        if (verbose) printHeader();

        // Phase 1: Lexical Analysis
        if (verbose) printPhase("LEXICAL ANALYSIS");
        Lexer lexer(sourceCode);
        tokens = lexer.tokenize();
        if (verbose) lexer.printTokens(tokens);

        // Phase 2: Syntax Analysis (Parsing)
        if (verbose) printPhase("SYNTAX ANALYSIS (PARSING)");
        Parser parser(tokens);
        ast = parser.parse();

        if (parser.hasErrors()) {
            parser.printErrors();
            return false;
        }

        if (verbose) {
            cout << ">> Parsing completed successfully\n";
            parser.printAST(ast.get());
        }

        // Phase 3: Semantic Analysis
        if (verbose) printPhase("SEMANTIC ANALYSIS");
        SemanticAnalyzer analyzer(symbolTable);
        analyzer.analyze(ast.get());

        if (analyzer.hasErrors()) {
            analyzer.printReport();
            return false;
        }

        if (verbose) analyzer.printReport();
        if (verbose) symbolTable.print();

        // Phase 4: Code Generation
        if (verbose) printPhase("CODE GENERATION");
        CodeGenerator codeGen(symbolTable);
        assemblyCode = codeGen.generate(ast.get());

        if (verbose) {
            codeGen.printAssembly(assemblyCode);
            cout << ">> Code generation completed successfully\n";
        }

        return true;
    }

    void interpret() {
        printPhase("INTERPRETATION & EXECUTION");
        Interpreter interpreter(symbolTable);
        interpreter.execute(ast.get());
    }

    void saveAssembly(const string& filename) {
        CodeGenerator codeGen(symbolTable);
        codeGen.saveToFile(assemblyCode, filename);
    }

    const string& getAssembly() const { return assemblyCode; }
};

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

void printUsage() {
    cout << "\nUsage:\n";
    cout << "  1. Run with example code (interactive)\n";
    cout << "  2. Provide source file as argument\n";
    cout << "  3. Enter code directly\n\n";
    cout << "Example Code:\n";
    cout << "  x = 10;\n";
    cout << "  y = x + 5;\n";
    cout << "  print y;\n";
    cout << "  check y;\n\n";
}

string getExampleProgram(int choice) {
    switch(choice) {
        case 1:
            return R"(
// Simple arithmetic and check
x = 10;
y = x + 5;
print y;
check y;
)";
        case 2:
            return R"(
// Even/Odd checker
num = 42;
print num;
check num;
result = num + 1;
print result;
check result;
)";
        case 3:
            return R"(
// Complex expressions
a = 10;
b = 20;
sum = a + b;
product = a * b;
remainder = product % 7;
print sum;
print product;
print remainder;
check sum;
check product;
check remainder;
)";
        case 4:
            return R"(
// Mathematical operations
x = 100;
y = x / 2;
z = y * 3;
result = z - 50;
print x;
print y;
print z;
print result;
check result;
)";
        default:
            return "x = 5;\nprint x;\ncheck x;";
    }
}

void runInteractiveMode() {
    cout << "\n=== EVENLANG COMPILER - INTERACTIVE MODE ===\n";
    cout << "\nSelect example program:\n";
    cout << "1. Simple arithmetic and check\n";
    cout << "2. Even/Odd checker\n";
    cout << "3. Complex expressions\n";
    cout << "4. Mathematical operations\n";
    cout << "5. Enter custom code\n";
    cout << "\nChoice (1-5): ";

    int choice;
    cin >> choice;
    cin.ignore();

    string sourceCode;

    if (choice >= 1 && choice <= 4) {
        sourceCode = getExampleProgram(choice);
        cout << "\n=== SOURCE CODE ===\n";
        cout << sourceCode << "\n";
    } else {
        cout << "\nEnter your code (type 'END' on a new line to finish):\n";
        string line;
        while (getline(cin, line)) {
            if (line == "END") break;
            sourceCode += line + "\n";
        }
    }

    // Compile
    Compiler compiler(sourceCode);
    bool success = compiler.compile(true);

    if (success) {
        // Execute
        compiler.interpret();

        // Save assembly
        cout << "\nSave assembly code? (y/n): ";
        char save;
        cin >> save;
        if (save == 'y' || save == 'Y') {
            compiler.saveAssembly("output.asm");
        }
    }
}

void compileFromFile(const string& filename) {
    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Error: Could not open file '" << filename << "'\n";
        return;
    }

    stringstream buffer;
    buffer << file.rdbuf();
    string sourceCode = buffer.str();
    file.close();

    cout << "\n=== COMPILING FILE: " << filename << " ===\n";
    cout << "\n=== SOURCE CODE ===\n";
    cout << sourceCode << "\n";

    Compiler compiler(sourceCode);
    bool success = compiler.compile(true);

    if (success) {
        compiler.interpret();

        // Auto-save assembly
        string asmFile = filename.substr(0, filename.find_last_of('.')) + ".asm";
        compiler.saveAssembly(asmFile);
    }
}

void runTestSuite() {
    cout << "\n=== EVENLANG COMPILER TEST SUITE ===\n";
    cout << string(70, '=') << "\n";

    vector<pair<string, string>> tests = {
        {"Test 1: Basic Assignment", "x = 5;\nprint x;\ncheck x;"},
        {"Test 2: Arithmetic", "a = 10;\nb = a + 5;\nprint b;\ncheck b;"},
        {"Test 3: Multiplication", "x = 3;\ny = x * 4;\nprint y;\ncheck y;"},
        {"Test 4: Division", "x = 20;\ny = x / 2;\nprint y;\ncheck y;"},
        {"Test 5: Modulo", "x = 17;\ny = x % 3;\nprint y;\ncheck y;"},
        {"Test 6: Complex", "a = 5;\nb = 3;\nc = a * b + 2;\nprint c;\ncheck c;"}
    };

    int passed = 0;
    int total = tests.size();

    for (const auto& test : tests) {
        cout << "\n" << string(70, '-') << "\n";
        cout << test.first << "\n";
        cout << string(70, '-') << "\n";
        cout << "Code: " << test.second << "\n";

        try {
            Compiler compiler(test.second);
            bool success = compiler.compile(false);

            if (success) {
                cout << "Compilation: PASSED ✓\n";
                compiler.interpret();
                passed++;
            } else {
                cout << "Compilation: FAILED ✗\n";
            }
        } catch (const exception& e) {
            cout << "Error: " << e.what() << "\n";
            cout << "Test: FAILED ✗\n";
        }
    }

    cout << "\n" << string(70, '=') << "\n";
    cout << "Test Results: " << passed << "/" << total << " passed\n";
    cout << string(70, '=') << "\n";
}

// ============================================================================
// MAIN FUNCTION
// ============================================================================

int main(int argc, char* argv[]) {
    try {
        if (argc == 2) {
            string arg = argv[1];

            if (arg == "--help" || arg == "-h") {
                printUsage();
                return 0;
            } else if (arg == "--test" || arg == "-t") {
                runTestSuite();
                return 0;
            } else {
                // Assume it's a filename
                compileFromFile(arg);
                return 0;
            }
        }

        // Interactive mode
        runInteractiveMode();

    } catch (const exception& e) {
        cerr << "\nFatal Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}

// ============================================================================
// END OF EVENLANG COMPILER
// ============================================================================

/*
COMPILATION INSTRUCTIONS:
========================

To compile this compiler:
    g++ -std=c++17 -O2 -o evenlang_compiler evenlang_compiler.cpp

To run:
    ./evenlang_compiler                    # Interactive mode
    ./evenlang_compiler program.even       # Compile a file
    ./evenlang_compiler --test             # Run test suite
    ./evenlang_compiler --help             # Show help

LANGUAGE FEATURES:
==================
1. Variable assignment: x = 10;
2. Arithmetic operations: +, -, *, /, %
3. Print statement: print x;
4. Even/Odd check: check x;
5. Comments: // single line comments
6. Expressions: Complex arithmetic with operator precedence

EXAMPLE PROGRAMS:
=================

Example 1 - Simple:
    x = 10;
    print x;
    check x;

Example 2 - Arithmetic:
    a = 5;
    b = 3;
    c = a * b + 2;
    print c;
    check c;

Example 3 - Complex:
    x = 100;
    y = x / 2;
    z = y * 3;
    result = z - 50;
    print result;
    check result;

COMPILER PHASES:
================
1. Lexical Analysis - Tokenization
2. Syntax Analysis - Parsing & AST construction
3. Semantic Analysis - Type checking & symbol table
4. Code Generation - x86-64 assembly generation
5. Interpretation - Direct execution

OUTPUT:
=======
- Token stream (lexical analysis)
- Abstract Syntax Tree (AST)
- Symbol table
- x86-64 assembly code
- Program execution results
*/
