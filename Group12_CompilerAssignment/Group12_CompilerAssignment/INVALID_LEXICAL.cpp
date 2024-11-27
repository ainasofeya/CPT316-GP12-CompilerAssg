// ************************ VALID SOURCE CODE *******************************
#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <unordered_map>
#include <fstream>

using namespace std;

//***************************************************************************
//_____________________________LEXICAL ANALYSIS______________________________
//***************************************************************************

// Define constants for the symbol table dimensions
const int size = 30, col = 2;
string symboltable[size][col];

// Enum to define the various token types in the language
enum class TokenType {
    Keyword, Identifier, Number, Operator, Punctuation, Invalid, StringLiteral, EndOfFile
};

// Structure to store token information such as type, value, line, and column
struct Token {
    TokenType type;
    string value;
    int line, column;

    Token(TokenType type, const string& value, int line, int column)
        : type(type), value(value), line(line), column(column) {}
};

// Define keywords, operators, and punctuations for MiniLang
const unordered_map<string, TokenType> keywords = {
     {"int", TokenType::Keyword}, // Defining 'int' as a keyword
     {"if", TokenType::Keyword}, // Defining 'if' as a keyword
     {"else", TokenType::Keyword}, // Defining 'else' as a keyword
     {"while", TokenType::Keyword}, // Defining 'while' as a keyword
     {"print", TokenType::Keyword} // Defining 'print' as a keyword
};
const unordered_map<char, TokenType> operators = {
    {'+', TokenType::Operator}, 
    {'*', TokenType::Operator}, 
    {'=', TokenType::Operator}  // Operators: +, *, and =
};

const unordered_map<char, TokenType> punctuation = {
    {';', TokenType::Punctuation},  // Punctuation: semicolon
    {'(', TokenType::Punctuation},  // Punctuation: open parenthesis
    {')', TokenType::Punctuation}, // Punctuation: close parenthesis
    {'{', TokenType::Punctuation}, // Punctuation: open curly bracket
    {'}', TokenType::Punctuation} // Punctuation: close curly bracket
};

// Lexer class for tokenizing the source code
class Lexer {
public:
    // Constructor to initialize the lexer with the source code string
    Lexer(const string& source)
        : source(source), pos(0), line(1), column(1) {}

    // Tokenizes the entire source code and returns a vector of tokens
    vector<Token> tokenize() {
        vector<Token> tokens;
        while (true) {
            Token token = nextToken();
            if (token.type == TokenType::EndOfFile) break;
            if (token.type == TokenType::Invalid) {
                cerr << "Lexical Error at line " << token.line << ", column " << token.column 
                     << ": Invalid token '" << token.value << "'\n";
                continue;
            }
            tokens.push_back(token);  // Add valid tokens to the list
        }
        return tokens;
    }

private:
    string source;  
    size_t pos;  
    int line, column;  

    // Peek the current character without advancing the position
    char peek() const {
        return (pos < source.size()) ? source[pos] : '\0';
    }

    // Advance to the next character and update line and column
    char advance() {
        char current = peek();
        pos++;
        column++;
        if (current == '\n') {
            line++;
            column = 1;
        }
        return current;
    }

    // Retrieves the next token from the source code
    Token nextToken() {
        skipWhitespace();  // Skip any whitespace
        char current = peek();

        if (current == '\0') return Token(TokenType::EndOfFile, "", line, column);  // End of file

        if (isalpha(current) || current == '_') return identifierOrKeyword();  // Identifier or keyword
        if (isdigit(current)) return number();  // Number
        if (operators.count(current)) return Token(operators.at(advance()), string(1, current), line, column - 1);  // Operator
        if (punctuation.count(current)) return Token(punctuation.at(advance()), string(1, current), line, column - 1);  // Punctuation
        if (current == '@' || current == '#' || current == '&') return symbol();  // Special symbols
        if (current == '"') return stringLiteral();  // String literal

        return Token(TokenType::Invalid, string(1, advance()), line, column - 1);  // Invalid token
    }

    // Skip over whitespace characters 
    void skipWhitespace() {
        while (isspace(peek())) advance();
    }

    // Process identifiers and keywords
    Token identifierOrKeyword() {
        int startColumn = column;
        string value;
        char firstChar = advance();  // Get first character of the identifier

        // Handle special characters used in identifiers
        if (firstChar == '@' || firstChar == '#' || firstChar == '&') {
            value += firstChar;
            while (isalnum(peek()) || peek() == '_') {
                value += advance();  // Read remaining characters of the identifier
            }
            return Token(TokenType::Invalid, value, line, startColumn);  // Special identifiers are invalid
        }

        value += firstChar;  // Append the first character to the value

        // Process the rest of the identifier
        while (isalnum(peek()) || peek() == '_') {
            value += advance();
        }

        // Return keyword if it exists in the predefined keywords list
        if (keywords.count(value)) return Token(TokenType::Keyword, value, line, startColumn);
        return Token(TokenType::Identifier, value, line, startColumn);  // Otherwise, it's an identifier
    }

    // Process numbers (digits)
    Token number() {
        int startColumn = column;
        string value;

        while (isdigit(peek())) {
            value += advance();  // Collect digits
        }

        // If a non-digit follows, the number is invalid
        if (isalpha(peek()) || peek() == '_') {
            while (isalnum(peek()) || peek() == '_') {
                value += advance();
            }
            return Token(TokenType::Invalid, value, line, startColumn);  // Invalid token
        }

        return Token(TokenType::Number, value, line, startColumn);  // Return a number token
    }

    // Process special symbols
    Token symbol() {
        int startColumn = column;
        string value;

        // Check for special symbols and collect them
        if (peek() == '@' || peek() == '#' || peek() == '&' || peek() == '$') {
            value += advance();
            while (isalnum(peek()) || peek() == '_') {
                value += advance();  // Collect alphanumeric characters
            }
            return Token(TokenType::Invalid, value, line, startColumn);  // Invalid token
        }

        return Token(TokenType::Invalid, value, line, startColumn);  // Return an invalid token for unrecognized symbols
    }

    // Process string literals (text enclosed in double quotes)
    Token stringLiteral() {
        int startColumn = column;
        string value;
        advance();  // Skip the opening double quote

        while (peek() != '"' && peek() != '\0') {  // Read characters until the closing quote or EOF
            value += advance();
        }

        if (peek() == '"') {
            advance();  // Skip the closing double quote
            return Token(TokenType::StringLiteral, value, line, startColumn);  // Return string literal token
        }

        return Token(TokenType::Invalid, "\"" + value, line, startColumn);  // Unterminated string literal
    }
};

// Helper function to convert TokenType enum to a string representation
string tokenTypeToString(TokenType type) {
    switch (type) {
        case TokenType::Keyword: return "Keyword";
        case TokenType::Identifier: return "Identifier";
        case TokenType::Number: return "Number";
        case TokenType::Operator: return "Operator";
        case TokenType::Punctuation: return "Punctuation";
        case TokenType::Invalid: return "Invalid";
        case TokenType::StringLiteral: return "String Literal";
        case TokenType::EndOfFile: return "EndOfFile";
    }
    return "Unknown";
}

// Utility function to display all tokens in the symbol table format
void printTokens(const vector<Token>& tokens) {
    cout << endl << "SYMBOL TABLE" << endl;
    int a = 0;
    for (const Token& token : tokens) {
        symboltable[a][0] = token.value;
        symboltable[a][1] = tokenTypeToString(token.type);
        cout << "Lexeme: " << symboltable[a][0] << endl << "Token: <" << symboltable[a][1] << ", " << symboltable[a][0] << ">" << endl << endl;
        a++;
    }
}
//***************************************************************************
//_____________________________SYNTAX ANALYSIS_______________________________
//***************************************************************************

// Function to parse an expression from the tokens (simple binary expression parsing)
struct ParseNode {
    string value;
    ParseNode* left;
    ParseNode* right;

    ParseNode(const string& value) : value(value), left(nullptr), right(nullptr) {}
};

// Function to check if the last token is a semicolon 
bool checkMissingSemicolon(const vector<Token>& tokens) {
    if (tokens.empty()) return true;

    const Token& lastToken = tokens.back();
    if (lastToken.type != TokenType::Punctuation || lastToken.value != ";") {
        cerr << "Syntax Error: Missing semicolon at line " << lastToken.line << ", column " << lastToken.column << endl;
        return true;  // Missing semicolon
    }
    return false;  // No error
}

// Function to parse the tokens into a parse tree (expression handling)
ParseNode* parseExpression(const vector<Token>& tokens) {
    vector<ParseNode*> nodes;

    // Convert tokens into parse nodes
    for (const Token& token : tokens) {
        if (token.type == TokenType::Identifier || token.type == TokenType::Number ||
            token.type == TokenType::Operator) {
            nodes.push_back(new ParseNode(token.value));  // Create a node for each token
        }
    }

    // Handle operator precedence ( *, +, = )
    for (const string& op : {"*", "+", "="}) {
        for (size_t i = 0; i < nodes.size(); ++i) {
            if (nodes[i]->value == op) {
                ParseNode* node = nodes[i];
                node->left = nodes[i - 1];
                node->right = nodes[i + 1];
                
                nodes.erase(nodes.begin() + i + 1);
                nodes.erase(nodes.begin() + i - 1);
                nodes[i - 1] = node;
                i--;
            }
        }
    }

    return nodes.empty() ? nullptr : nodes[0];
}

// Helper function to recursively generate the parse tree in DOT format
void generateParseTree(ParseNode* root, ofstream& file, int& counter) {
    if (!root) return;

    int current = counter++;
    file << "    Node" << current << " [label=\"" << root->value << "\"];\n";

    if (root->left) {
        int left = counter;
        generateParseTree(root->left, file, counter);
        file << "    Node" << current << " -> Node" << left << ";\n";
    }

    if (root->right) {
        int right = counter;
        generateParseTree(root->right, file, counter);
        file << "    Node" << current << " -> Node" << right << ";\n";
    }
}

// Function to build and save the parse tree to a DOT file
void buildAndSaveParseTree(const vector<Token>& tokens) {
    ofstream file("PARSETREE1.dot");
    if (!file.is_open()) {
        cerr << "Error: Unable to create DOT file!" << endl;
        return;
    }

    file << "digraph ParseTree {\n";
    file << "    node [shape=ellipse];\n";

    int counter = 0;
    ParseNode* root = parseExpression(tokens);
    generateParseTree(root, file, counter);  // Recursively generate the tree

    file << "}\n";
    file.close();

    cout << "Parse tree saved to 'PARSETREE1.dot'. Render using Graphviz: dot -Tpng PARSETREE1.dot -o PARSETREE1.png\n";
}

int main() {
    // MiniLang test case -> invalid lexical
    string sourceCode = R"(
        int @x=y+z*z;
    )";

    Lexer lexer(sourceCode);  // Initialize the lexer
    vector<Token> tokens = lexer.tokenize();  // Tokenize the source code

    printTokens(tokens);  // Display all the tokens in the symbol table
    
    // Check for missing semicolon and only generate parse tree if no error
    if (!checkMissingSemicolon(tokens)) {
        buildAndSaveParseTree(tokens);  // Build and save the parse tree only if no semicolon error
    }

    return 0;
}

