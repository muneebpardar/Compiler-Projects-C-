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

// ============================================================================
// TOKEN DEFINITIONS - Lexical Analysis Phase
// ============================================================================

enum class TokenType {
    // Keywords
    PRINT, MAX,

    // Identifiers and Literals
    IDENTIFIER, NUMBER,

    // Operators and Delimiters
    ASSIGN, SEMICOLON, LPAREN, RPAREN, COMMA,
    PLUS, MINUS, MULTIPLY, DIVIDE,

    // Special
    END_OF_FILE, INVALID
};

struct Token {
    TokenType type;
    std::string lexeme;
    int value;
    int line;
    int column;

    Token(TokenType t, const std::string& lex, int val = 0, int ln = 1, int col = 1)
        : type(t), lexeme(lex), value(val), line(ln), column(col) {}

    std::string toString() const {
        std::stringstream ss;
        ss << "Token(" << tokenTypeToString(type) << ", '" << lexeme << "'";
        if (type == TokenType::NUMBER) ss << ", value=" << value;
        ss << ", line=" << line << ", col=" << column << ")";
        return ss.str();
    }

    static std::string tokenTypeToString(TokenType type) {
        switch(type) {
            case TokenType::PRINT: return "PRINT";
            case TokenType::MAX: return "MAX";
            case TokenType::IDENTIFIER: return "IDENTIFIER";
            case TokenType::NUMBER: return "NUMBER";
            case TokenType::ASSIGN: return "ASSIGN";
            case TokenType::SEMICOLON: return "SEMICOLON";
            case TokenType::LPAREN: return "LPAREN";
            case TokenType::RPAREN: return "RPAREN";
            case TokenType::COMMA: return "COMMA";
            case TokenType::PLUS: return "PLUS";
            case TokenType::MINUS: return "MINUS";
            case TokenType::MULTIPLY: return "MULTIPLY";
            case TokenType::DIVIDE: return "DIVIDE";
            case TokenType::END_OF_FILE: return "EOF";
            case TokenType::INVALID: return "INVALID";
            default: return "UNKNOWN";
        }
    }
};

// ============================================================================
// LEXER - Tokenization Phase
// ============================================================================

class Lexer {
private:
    std::string source;
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
        while (currentChar != '\0' && std::isspace(currentChar)) {
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
        std::string numStr;
        while (currentChar != '\0' && std::isdigit(currentChar)) {
            numStr += currentChar;
            advance();
        }
        return Token(TokenType::NUMBER, numStr, std::stoi(numStr), line, startCol);
    }

    Token identifier() {
        int startCol = column;
        std::string id;
        while (currentChar != '\0' && (std::isalnum(currentChar) || currentChar == '_')) {
            id += currentChar;
            advance();
        }

        // Check for keywords
        if (id == "print") return Token(TokenType::PRINT, id, 0, line, startCol);
        if (id == "max") return Token(TokenType::MAX, id, 0, line, startCol);

        return Token(TokenType::IDENTIFIER, id, 0, line, startCol);
    }

public:
    Lexer(const std::string& src)
        : source(src), position(0), line(1), column(1) {
        currentChar = source.empty() ? '\0' : source[0];
    }

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;

        while (currentChar != '\0') {
            skipWhitespace();

            if (currentChar == '\0') break;

            // Skip comments before processing other tokens
            if (currentChar == '/' && peek() == '/') {
                skipComment();
                continue;
            }

            int startCol = column;

            // Numbers
            if (std::isdigit(currentChar)) {
                tokens.push_back(number());
            }
            // Identifiers and Keywords
            else if (std::isalpha(currentChar) || currentChar == '_') {
                tokens.push_back(identifier());
            }
            // Operators and Delimiters
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
            else if (currentChar == ',') {
                tokens.push_back(Token(TokenType::COMMA, ",", 0, line, startCol));
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
            else {
                // Skip invalid characters silently (like stray newlines)
                advance();
            }
        }

        tokens.push_back(Token(TokenType::END_OF_FILE, "", 0, line, column));
        return tokens;
    }

    void printTokens(const std::vector<Token>& tokens) {
        std::cout << "\n=== LEXICAL ANALYSIS - TOKEN STREAM ===\n";
        std::cout << std::string(60, '=') << "\n";
        for (const auto& token : tokens) {
            std::cout << token.toString() << "\n";
        }
        std::cout << std::string(60, '=') << "\n";
    }
};

// ============================================================================
// AST NODE DEFINITIONS - Syntax Analysis Phase
// ============================================================================

enum class ASTNodeType {
    PROGRAM, ASSIGNMENT, PRINT_STMT,
    BINARY_OP, MAX_CALL, IDENTIFIER, NUMBER
};

class ASTNode {
public:
    ASTNodeType type;
    virtual ~ASTNode() = default;
    virtual std::string toString(int indent = 0) const = 0;

protected:
    std::string getIndent(int level) const {
        return std::string(level * 2, ' ');
    }
};

class NumberNode : public ASTNode {
public:
    int value;

    NumberNode(int val) : value(val) {
        type = ASTNodeType::NUMBER;
    }

    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "NumberNode(" + std::to_string(value) + ")";
    }
};

class IdentifierNode : public ASTNode {
public:
    std::string name;

    IdentifierNode(const std::string& n) : name(n) {
        type = ASTNodeType::IDENTIFIER;
    }

    std::string toString(int indent = 0) const override {
        return getIndent(indent) + "IdentifierNode('" + name + "')";
    }
};

class BinaryOpNode : public ASTNode {
public:
    std::string op;
    std::unique_ptr<ASTNode> left;
    std::unique_ptr<ASTNode> right;

    BinaryOpNode(const std::string& operation,
                 std::unique_ptr<ASTNode> l,
                 std::unique_ptr<ASTNode> r)
        : op(operation), left(std::move(l)), right(std::move(r)) {
        type = ASTNodeType::BINARY_OP;
    }

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "BinaryOpNode('" + op + "')\n";
        result += getIndent(indent + 1) + "Left:\n" + left->toString(indent + 2) + "\n";
        result += getIndent(indent + 1) + "Right:\n" + right->toString(indent + 2);
        return result;
    }
};

class MaxCallNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> arg1;
    std::unique_ptr<ASTNode> arg2;

    MaxCallNode(std::unique_ptr<ASTNode> a1, std::unique_ptr<ASTNode> a2)
        : arg1(std::move(a1)), arg2(std::move(a2)) {
        type = ASTNodeType::MAX_CALL;
    }

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "MaxCallNode\n";
        result += getIndent(indent + 1) + "Arg1:\n" + arg1->toString(indent + 2) + "\n";
        result += getIndent(indent + 1) + "Arg2:\n" + arg2->toString(indent + 2);
        return result;
    }
};

class AssignmentNode : public ASTNode {
public:
    std::string varName;
    std::unique_ptr<ASTNode> expression;

    AssignmentNode(const std::string& name, std::unique_ptr<ASTNode> expr)
        : varName(name), expression(std::move(expr)) {
        type = ASTNodeType::ASSIGNMENT;
    }

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "AssignmentNode('" + varName + "')\n";
        result += getIndent(indent + 1) + "Expression:\n" + expression->toString(indent + 2);
        return result;
    }
};

class PrintNode : public ASTNode {
public:
    std::unique_ptr<ASTNode> expression;

    PrintNode(std::unique_ptr<ASTNode> expr)
        : expression(std::move(expr)) {
        type = ASTNodeType::PRINT_STMT;
    }

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "PrintNode\n";
        result += getIndent(indent + 1) + "Expression:\n" + expression->toString(indent + 2);
        return result;
    }
};

class ProgramNode : public ASTNode {
public:
    std::vector<std::unique_ptr<ASTNode>> statements;

    ProgramNode() {
        type = ASTNodeType::PROGRAM;
    }

    void addStatement(std::unique_ptr<ASTNode> stmt) {
        statements.push_back(std::move(stmt));
    }

    std::string toString(int indent = 0) const override {
        std::string result = getIndent(indent) + "ProgramNode\n";
        for (size_t i = 0; i < statements.size(); i++) {
            result += getIndent(indent + 1) + "Statement " + std::to_string(i + 1) + ":\n";
            result += statements[i]->toString(indent + 2);
            if (i < statements.size() - 1) result += "\n";
        }
        return result;
    }
};

// ============================================================================
// PARSER - Syntax Analysis with AST Generation
// ============================================================================

class Parser {
private:
    std::vector<Token> tokens;
    size_t current;
    std::vector<std::string> errors;

    Token currentToken() {
        return tokens[current];
    }

    Token peek(int offset = 1) {
        size_t pos = current + offset;
        return (pos < tokens.size()) ? tokens[pos] : tokens.back();
    }

    void advance() {
        if (current < tokens.size() - 1) {
            current++;
        }
    }

    bool match(TokenType type) {
        if (currentToken().type == type) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType type, const std::string& message) {
        if (currentToken().type != type) {
            error(message);
            throw std::runtime_error(message);
        }
        advance();
    }

    void error(const std::string& message) {
        Token tok = currentToken();
        std::stringstream ss;
        ss << "Parse Error at line " << tok.line << ", column " << tok.column
           << ": " << message << " (found '" << tok.lexeme << "')";
        errors.push_back(ss.str());
    }

    std::unique_ptr<ASTNode> parseExpression() {
        return parseAdditive();
    }

    std::unique_ptr<ASTNode> parseAdditive() {
        auto left = parseMultiplicative();

        while (currentToken().type == TokenType::PLUS ||
               currentToken().type == TokenType::MINUS) {
            std::string op = currentToken().lexeme;
            advance();
            auto right = parseMultiplicative();
            left = std::make_unique<BinaryOpNode>(op, std::move(left), std::move(right));
        }

        return left;
    }

    std::unique_ptr<ASTNode> parseMultiplicative() {
        auto left = parsePrimary();

        while (currentToken().type == TokenType::MULTIPLY ||
               currentToken().type == TokenType::DIVIDE) {
            std::string op = currentToken().lexeme;
            advance();
            auto right = parsePrimary();
            left = std::make_unique<BinaryOpNode>(op, std::move(left), std::move(right));
        }

        return left;
    }

    std::unique_ptr<ASTNode> parsePrimary() {
        Token tok = currentToken();

        // Number
        if (tok.type == TokenType::NUMBER) {
            advance();
            return std::make_unique<NumberNode>(tok.value);
        }

        // Max function call
        if (tok.type == TokenType::MAX) {
            return parseMaxCall();
        }

        // Identifier
        if (tok.type == TokenType::IDENTIFIER) {
            advance();
            return std::make_unique<IdentifierNode>(tok.lexeme);
        }

        // Parenthesized expression
        if (tok.type == TokenType::LPAREN) {
            advance();
            auto expr = parseExpression();
            expect(TokenType::RPAREN, "Expected ')' after expression");
            return expr;
        }

        error("Expected expression");
        throw std::runtime_error("Expected expression");
    }

    std::unique_ptr<ASTNode> parseMaxCall() {
        expect(TokenType::MAX, "Expected 'max'");
        expect(TokenType::LPAREN, "Expected '(' after 'max'");

        auto arg1 = parseExpression();
        expect(TokenType::COMMA, "Expected ',' between max arguments");
        auto arg2 = parseExpression();

        expect(TokenType::RPAREN, "Expected ')' after max arguments");

        return std::make_unique<MaxCallNode>(std::move(arg1), std::move(arg2));
    }

    std::unique_ptr<ASTNode> parseAssignment() {
        Token idToken = currentToken();
        expect(TokenType::IDENTIFIER, "Expected identifier");

        expect(TokenType::ASSIGN, "Expected '=' in assignment");

        auto expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after assignment");

        return std::make_unique<AssignmentNode>(idToken.lexeme, std::move(expr));
    }

    std::unique_ptr<ASTNode> parsePrintStatement() {
        expect(TokenType::PRINT, "Expected 'print'");

        auto expr = parseExpression();
        expect(TokenType::SEMICOLON, "Expected ';' after print statement");

        return std::make_unique<PrintNode>(std::move(expr));
    }

    std::unique_ptr<ASTNode> parseStatement() {
        Token tok = currentToken();

        if (tok.type == TokenType::PRINT) {
            return parsePrintStatement();
        }
        else if (tok.type == TokenType::IDENTIFIER) {
            return parseAssignment();
        }
        else if (tok.type == TokenType::END_OF_FILE) {
            return nullptr;
        }
        else {
            error("Expected statement");
            advance(); // Skip invalid token
            return nullptr;
        }
    }

public:
    Parser(const std::vector<Token>& toks) : tokens(toks), current(0) {}

    std::unique_ptr<ProgramNode> parse() {
        auto program = std::make_unique<ProgramNode>();

        while (currentToken().type != TokenType::END_OF_FILE) {
            try {
                auto stmt = parseStatement();
                if (stmt) {
                    program->addStatement(std::move(stmt));
                }
            } catch (const std::exception& e) {
                // Error recovery: skip to next statement
                while (currentToken().type != TokenType::SEMICOLON &&
                       currentToken().type != TokenType::END_OF_FILE) {
                    advance();
                }
                if (currentToken().type == TokenType::SEMICOLON) {
                    advance();
                }
            }
        }

        return program;
    }

    bool hasErrors() const {
        return !errors.empty();
    }

    void printErrors() const {
        if (!errors.empty()) {
            std::cout << "\n=== PARSE ERRORS ===\n";
            for (const auto& err : errors) {
                std::cout << err << "\n";
            }
        }
    }

    void printAST(const ProgramNode* program) {
        std::cout << "\n=== ABSTRACT SYNTAX TREE (AST) ===\n";
        std::cout << std::string(60, '=') << "\n";
        std::cout << program->toString() << "\n";
        std::cout << std::string(60, '=') << "\n";
    }
};

// ============================================================================
// SYMBOL TABLE - Semantic Analysis Phase
// ============================================================================

struct SymbolInfo {
    std::string name;
    std::string type;
    int value;
    bool initialized;
    int declarationLine;

    SymbolInfo(const std::string& n, const std::string& t, int line)
        : name(n), type(t), value(0), initialized(false), declarationLine(line) {}
};

class SymbolTable {
private:
    std::map<std::string, SymbolInfo> symbols;
    int currentScope;

public:
    SymbolTable() : currentScope(0) {}

    void declare(const std::string& name, const std::string& type, int line) {
        if (symbols.find(name) != symbols.end()) {
            throw std::runtime_error("Variable '" + name + "' already declared");
        }
        symbols.insert({name, SymbolInfo(name, type, line)});
    }

    void set(const std::string& name, int value) {
        auto it = symbols.find(name);
        if (it == symbols.end()) {
            // Auto-declare on first assignment
            symbols.insert({name, SymbolInfo(name, "int", 0)});
            it = symbols.find(name);
        }
        it->second.value = value;
        it->second.initialized = true;
    }

    int get(const std::string& name) {
        auto it = symbols.find(name);
        if (it == symbols.end()) {
            throw std::runtime_error("Variable '" + name + "' not declared");
        }
        if (!it->second.initialized) {
            throw std::runtime_error("Variable '" + name + "' used before initialization");
        }
        return it->second.value;
    }

    bool exists(const std::string& name) const {
        return symbols.find(name) != symbols.end();
    }

    bool isInitialized(const std::string& name) const {
        auto it = symbols.find(name);
        return it != symbols.end() && it->second.initialized;
    }

    void print() const {
        std::cout << "\n=== SYMBOL TABLE ===\n";
        std::cout << std::string(80, '=') << "\n";
        std::cout << std::left << std::setw(15) << "Symbol"
                  << std::setw(10) << "Type"
                  << std::setw(12) << "Value"
                  << std::setw(15) << "Initialized"
                  << std::setw(10) << "Decl Line" << "\n";
        std::cout << std::string(80, '-') << "\n";

        for (const auto& pair : symbols) {
            const SymbolInfo& info = pair.second;
            std::cout << std::left << std::setw(15) << info.name
                      << std::setw(10) << info.type
                      << std::setw(12) << (info.initialized ? std::to_string(info.value) : "undefined")
                      << std::setw(15) << (info.initialized ? "Yes" : "No")
                      << std::setw(10) << info.declarationLine << "\n";
        }
        std::cout << std::string(80, '=') << "\n";
    }
};

// ============================================================================
// SEMANTIC ANALYZER - Type Checking and Scope Management
// ============================================================================

class SemanticAnalyzer {
private:
    SymbolTable& symbolTable;
    std::vector<std::string> errors;
    std::vector<std::string> warnings;

    void error(const std::string& message) {
        errors.push_back("Semantic Error: " + message);
    }

    void warning(const std::string& message) {
        warnings.push_back("Warning: " + message);
    }

    void analyzeExpression(ASTNode* node) {
        if (!node) return;

        if (node->type == ASTNodeType::IDENTIFIER) {
            IdentifierNode* idNode = static_cast<IdentifierNode*>(node);
            if (!symbolTable.exists(idNode->name)) {
                error("Variable '" + idNode->name + "' not declared");
            }
            // Note: We don't check initialization here because variables
            // are considered initialized when assigned, and we're analyzing
            // after declaration but during the same pass
        }
        else if (node->type == ASTNodeType::BINARY_OP) {
            BinaryOpNode* binOp = static_cast<BinaryOpNode*>(node);
            analyzeExpression(binOp->left.get());
            analyzeExpression(binOp->right.get());
        }
        else if (node->type == ASTNodeType::MAX_CALL) {
            MaxCallNode* maxNode = static_cast<MaxCallNode*>(node);
            analyzeExpression(maxNode->arg1.get());
            analyzeExpression(maxNode->arg2.get());
        }
    }

    void analyzeStatement(ASTNode* stmt) {
        if (stmt->type == ASTNodeType::ASSIGNMENT) {
            AssignmentNode* assign = static_cast<AssignmentNode*>(stmt);
            // First declare the variable being assigned to (if not already declared)
            if (!symbolTable.exists(assign->varName)) {
                symbolTable.declare(assign->varName, "int", 0);
            }
            // Then analyze the expression on the right-hand side
            analyzeExpression(assign->expression.get());
        }
        else if (stmt->type == ASTNodeType::PRINT_STMT) {
            PrintNode* print = static_cast<PrintNode*>(stmt);
            analyzeExpression(print->expression.get());
        }
    }

public:
    SemanticAnalyzer(SymbolTable& st) : symbolTable(st) {}

    void analyze(ProgramNode* program) {
        for (const auto& stmt : program->statements) {
            analyzeStatement(stmt.get());
        }
    }

    bool hasErrors() const {
        return !errors.empty();
    }

    void printReport() const {
        if (!errors.empty()) {
            std::cout << "\n=== SEMANTIC ERRORS ===\n";
            for (const auto& err : errors) {
                std::cout << err << "\n";
            }
        }

        if (!warnings.empty()) {
            std::cout << "\n=== WARNINGS ===\n";
            for (const auto& warn : warnings) {
                std::cout << warn << "\n";
            }
        }

        if (errors.empty() && warnings.empty()) {
            std::cout << "\n=== SEMANTIC ANALYSIS ===\n";
            std::cout << ">> No semantic errors found\n";
            std::cout << ">> Type checking passed\n";
            std::cout << ">> All variables properly declared\n";
        }
    }
};

// ============================================================================
// INTERPRETER - Execution Engine
// ============================================================================

class Interpreter {
private:
    SymbolTable& symbolTable;
    std::vector<std::string> output;

    int evaluateExpression(ASTNode* node) {
        if (!node) return 0;

        switch (node->type) {
            case ASTNodeType::NUMBER: {
                NumberNode* numNode = static_cast<NumberNode*>(node);
                return numNode->value;
            }

            case ASTNodeType::IDENTIFIER: {
                IdentifierNode* idNode = static_cast<IdentifierNode*>(node);
                return symbolTable.get(idNode->name);
            }

            case ASTNodeType::BINARY_OP: {
                BinaryOpNode* binOp = static_cast<BinaryOpNode*>(node);
                int left = evaluateExpression(binOp->left.get());
                int right = evaluateExpression(binOp->right.get());

                if (binOp->op == "+") return left + right;
                if (binOp->op == "-") return left - right;
                if (binOp->op == "*") return left * right;
                if (binOp->op == "/") {
                    if (right == 0) throw std::runtime_error("Division by zero");
                    return left / right;
                }
                break;
            }

            case ASTNodeType::MAX_CALL: {
                MaxCallNode* maxNode = static_cast<MaxCallNode*>(node);
                int val1 = evaluateExpression(maxNode->arg1.get());
                int val2 = evaluateExpression(maxNode->arg2.get());
                return (val1 > val2) ? val1 : val2;
            }

            default:
                break;
        }

        return 0;
    }

    void executeStatement(ASTNode* stmt) {
        if (stmt->type == ASTNodeType::ASSIGNMENT) {
            AssignmentNode* assign = static_cast<AssignmentNode*>(stmt);
            int value = evaluateExpression(assign->expression.get());
            symbolTable.set(assign->varName, value);
        }
        else if (stmt->type == ASTNodeType::PRINT_STMT) {
            PrintNode* print = static_cast<PrintNode*>(stmt);
            int value = evaluateExpression(print->expression.get());
            output.push_back(std::to_string(value));
            std::cout << value << std::endl;
        }
    }

public:
    Interpreter(SymbolTable& st) : symbolTable(st) {}

    void execute(ProgramNode* program) {
        std::cout << "\n=== PROGRAM EXECUTION ===\n";
        std::cout << std::string(60, '=') << "\n";
        std::cout << "Output:\n";

        for (const auto& stmt : program->statements) {
            executeStatement(stmt.get());
        }

        std::cout << std::string(60, '=') << "\n";
    }

    const std::vector<std::string>& getOutput() const {
        return output;
    }
};

// ============================================================================
// MAIN COMPILER DRIVER
// ============================================================================

class MaxLangCompiler {
private:
    std::string sourceCode;

public:
    void compile(const std::string& source) {
        sourceCode = source;

        std::cout << "\n";
        std::cout << "============================================================\n";
        std::cout << "           MaxLang Compiler - v1.0                          \n";
        std::cout << "           Maximum Finder Language Compiler                 \n";
        std::cout << "============================================================\n";

        std::cout << "\n=== SOURCE CODE ===\n";
        std::cout << std::string(60, '=') << "\n";
        std::cout << source << "\n";
        std::cout << std::string(60, '=') << "\n";

        try {
            // Phase 1: Lexical Analysis
            std::cout << "\n[Phase 1] Starting Lexical Analysis...\n";
            Lexer lexer(source);
            std::vector<Token> tokens = lexer.tokenize();
            lexer.printTokens(tokens);
            std::cout << ">> Lexical Analysis Complete - " << tokens.size() << " tokens generated\n";

            // Phase 2: Syntax Analysis (Parsing)
            std::cout << "\n[Phase 2] Starting Syntax Analysis (Parsing)...\n";
            Parser parser(tokens);
            auto ast = parser.parse();

            if (parser.hasErrors()) {
                parser.printErrors();
                return;
            }

            parser.printAST(ast.get());
            std::cout << ">> Syntax Analysis Complete - AST generated\n";

            // Phase 3: Semantic Analysis
            std::cout << "\n[Phase 3] Starting Semantic Analysis...\n";
            SymbolTable symbolTable;
            SemanticAnalyzer analyzer(symbolTable);
            analyzer.analyze(ast.get());
            analyzer.printReport();

            if (analyzer.hasErrors()) {
                std::cout << "\n>> Compilation failed due to semantic errors\n";
                return;
            }

            std::cout << ">> Semantic Analysis Complete\n";

            // Phase 4: Execution (Interpretation)
            std::cout << "\n[Phase 4] Starting Program Execution...\n";
            Interpreter interpreter(symbolTable);
            interpreter.execute(ast.get());

            // Display Symbol Table
            symbolTable.print();

            std::cout << "\n";
            std::cout << "============================================================\n";
            std::cout << "  >> Compilation and Execution Successful!                  \n";
            std::cout << "============================================================\n";

        } catch (const std::exception& e) {
            std::cout << "\n✗ FATAL ERROR: " << e.what() << "\n";
        }
    }
};

// ============================================================================
// TEST SUITE
// ============================================================================

void runTests() {
    MaxLangCompiler compiler;

    std::cout << "\n";
    std::cout << "============================================================\n";
    std::cout << "              MaxLang Compiler Test Suite                   \n";
    std::cout << "============================================================\n";

    // Test 1: Basic max function
    std::cout << "\n\n**************** TEST 1: Basic Max Function ****************\n";
    compiler.compile("x = 5;\ny = 3;\nprint max(x, y);");

    // Test 2: Max with expressions
    std::cout << "\n\n**************** TEST 2: Max with Arithmetic ****************\n";
    compiler.compile("a = 10;\nb = 20;\nprint max(a + 5, b - 3);");

    // Test 3: Nested max calls
    std::cout << "\n\n**************** TEST 3: Nested Max Calls ****************\n";
    compiler.compile("x = 5;\ny = 10;\nz = 7;\nprint max(max(x, y), z);");

    // Test 4: Multiple print statements
    std::cout << "\n\n**************** TEST 4: Multiple Statements ****************n";
    compiler.compile("num1 = 100;\nnum2 = 50;\nprint num1;\nprint num2;\nprint max(num1, num2);");

    // Test 5: Complex arithmetic expressions
    std::cout << "\n\n**************** TEST 5: Complex Expressions ****************\n";
    compiler.compile("x = 2 * 3 + 4;\ny = 10 - 2 * 2;\nprint max(x, y);");

    // Test 6: Error - Undefined variable
    std::cout << "\n\n**************** TEST 6: Undefined Variable Error ****************\n";
    compiler.compile("print max(x, y);");

    // Test 7: Comments support
    std::cout << "\n\n**************** TEST 7: Comments Support ****************\n";
    compiler.compile("// This is a comment\nx = 15;\n// Another comment\ny = 25;\nprint max(x, y);");

    // Test 8: Single variable
    std::cout << "\n\n**************** TEST 8: Direct Numbers ****************\n";
    compiler.compile("print max(42, 17);");

    // Test 9: Equal values
    std::cout << "\n\n**************** TEST 9: Equal Values ****************\n";
    compiler.compile("a = 100;\nb = 100;\nprint max(a, b);");

    // Test 10: Complex nested expression
    std::cout << "\n\n**************** TEST 10: Complex Nested ****************\n";
    compiler.compile("w = 5;\nx = 10;\ny = 15;\nz = 20;\nresult = max(w + x, y * 2);\nprint result;\nprint max(result, z);");
}

// ============================================================================
// MAIN FUNCTION
// ============================================================================

int main(int argc, char* argv[]) {
    // Check if file argument provided
    if (argc > 1) {
        // File mode: compile file from command line argument
        std::string filename = argv[1];
        std::ifstream file(filename);

        if (!file.is_open()) {
            std::cerr << "Error: Could not open file '" << filename << "'\n";
            return 1;
        }

        std::string source((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());
        file.close();

        MaxLangCompiler compiler;
        compiler.compile(source);
        return 0;
    }

    // Run comprehensive test suite
    runTests();

    // Interactive mode
    std::cout << "\n\n";
    std::cout << "============================================================\n";
    std::cout << "         Interactive Mode - Enter Your MaxLang Code         \n";
    std::cout << "         (Type 'exit' on a new line to quit)                \n";
    std::cout << "============================================================\n\n";

    std::string line, code;
    std::cout << "MaxLang> ";

    while (std::getline(std::cin, line)) {
        if (line == "exit" || line == "quit") {
            break;
        }

        if (line.empty()) {
            if (!code.empty()) {
                MaxLangCompiler compiler;
                compiler.compile(code);
                code.clear();
            }
            std::cout << "\nMaxLang> ";
            continue;
        }

        code += line + "\n";
        std::cout << "       > ";
    }

    std::cout << "\nGoodbye! Thank you for using MaxLang Compiler.\n";

    return 0;
}
