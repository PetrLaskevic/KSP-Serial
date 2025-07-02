#include <cctype>
#include <iostream>
#include <optional>
#include <vector>
#include <string>
#include <array>
#include <cctype>
#include <sstream>
#include <assert.h>
#include <sstream>
#include <algorithm>

/*
Zadani zde: https://ksp.mff.cuni.cz/h/ulohy/37/zadani2.html#task-37-2-S
Program je Lexer, ktery se snazi vstupni text prevest na seznam tokenu,
tj. "pospojovat souvisejici znaky dohromady"
Kompilovano stejne jako predchozi v C++20, v g++, na Linuxu
*/

enum TokenType {
  // Operátory
  TK_NOT, TK_EQUAL, TK_GREATER, TK_LESS, TK_MINUS,
  TK_PLUS, TK_SEMICOLON, TK_SLASH, TK_STAR,
  TK_NOT_EQUAL, TK_EQUAL_EQUAL, TK_GREATER_EQUAL,
  TK_LESS_EQUAL, TK_LBRACE, TK_LPAREN, TK_RBRACE,
  TK_RPAREN,

  //[] závorky pro indexování stringů
  TK_L_SQ_BRACKET, TK_R_SQ_BRACKET,
  // Boolean operátory
  TK_AND, TK_OR,

  // Klíčová slova
  TK_ELSE, TK_FOR, TK_IF, TK_PRINT, TK_VAR,
  TK_WHILE, TK_FN, TK_RETURN, TK_COMMA,

  // Literály
  TK_NAME, TK_NUMBER, TK_STRING,
  
  // poslední token, značí konec souboru
  TK_EOF
};


struct Token {
  TokenType type;

  // pouze u TK_NAME a TK_NUMBER a TK_STRING, jinak ""
  std::string value;

  int row;
  int column;

  //cool, ze je v tomhle konstruktoru string v optional
  //(a nepotrebuju tedy psat Token(TokenType t): type{t}, value{""} {} )
  //dokonce by to byla chyba:
  //more than one instance of constructor "Token::Token" matches the argument list:
  Token(TokenType t, std::string v = "", int r = -1, int c = -1)
      : type{t}, value{v}, row(r), column(c) {}
 
};


std::string token_type_to_str(TokenType t) {
  switch (t) {
    case  TK_NOT: return "NOT";
    case  TK_EQUAL: return "EQUAL";
    case  TK_GREATER: return "GREATER";
    case  TK_LESS: return "LESS";
    case  TK_MINUS: return "MINUS";
    case  TK_PLUS: return "PLUS";
    case  TK_SEMICOLON: return "SEMICOLON";
    case  TK_SLASH: return "SLASH";
    case  TK_STAR: return "STAR";
    case  TK_NOT_EQUAL: return "NOT_EQUAL";
    case  TK_EQUAL_EQUAL: return "EQUAL_EQUAL";
    case  TK_GREATER_EQUAL: return "GREATER_EQUAL";
    case  TK_LESS_EQUAL: return "LESS_EQUAL";
    case  TK_LBRACE: return "LBRACE";
    case  TK_LPAREN: return "LPAREN";
    case  TK_RBRACE: return "RBRACE";
    case  TK_RPAREN: return "RPAREN";
    case  TK_AND: return "AND";
    case  TK_OR: return "OR";
    case  TK_ELSE: return "ELSE";
    case  TK_FOR: return "FOR";
    case  TK_IF: return "IF";
    case  TK_PRINT: return "PRINT";
    case  TK_VAR: return "VAR";
    case  TK_WHILE: return "WHILE";
    case  TK_NAME: return "NAME";
    case  TK_NUMBER: return "NUMBER";
    case TK_FN: return "FN";
    case TK_COMMA: return "COMMA";
    case TK_RETURN: return "RETURN";
    case TK_STRING: return "STRING";
  }
  return "<invalid token value>";
}

std::string decoratedLine(std::string text, int row, int column){
    // find nth line
    std::stringstream test(text);
    std::string segment;
    std::vector<std::string> seglist;
    while(std::getline(test, segment, '\n')){
        seglist.push_back(segment);
    }
    segment = seglist[row];
    segment.insert(column, "\u001b[31m"); //ANSI escape code for red
    size_t resetPosition = std::min(static_cast<size_t>(column + 6), segment.size()); //+7
    segment.insert(resetPosition, "\u001b[0m"); //reset color
    return segment;
}

struct Scanner {
  std::string source;
  std::string originalInput; //source pred modifikacemi, pro cerr
  int row = 0;
  int column = 0;

  Scanner(std::string s) : source{s}, originalInput(s) {}

  void advance(size_t i = 1) {
    if(source[0] == '\n'){
        row++;
        column = 0;
    }else{
        column++;
    }
    source.erase(0, i); 
  }
  
  bool is_at_end() { return source.empty(); }

  char peek() {
    return source.empty() ? '\0' : source[0];
  }

  bool match(char c) {
    if (peek() == c) {
      advance();
      return true;
    }
    return false;
  }
};

std::optional<Token>
match_operator_token(Scanner &sc) {
    int beganTokenAtCol = sc.column;
    if(sc.match('!')){
        if(sc.match('=')){
            return Token(TK_NOT_EQUAL, "", sc.row, beganTokenAtCol);
        }else{
            return Token(TK_NOT, "", sc.row, beganTokenAtCol);
        }
    }
    if(sc.match('=')){
        if(sc.match('=')){
            return Token(TK_EQUAL_EQUAL, "", sc.row, beganTokenAtCol);
        }else{
            return Token(TK_EQUAL, "", sc.row, beganTokenAtCol);
        }
    }
    if(sc.match('>')){
        if(sc.match('=')){
            return Token(TK_GREATER_EQUAL, "", sc.row, beganTokenAtCol);
        }else{
            return Token(TK_GREATER, "", sc.row, beganTokenAtCol);
        }
    }
    if(sc.match('<')){
        if(sc.match('=')){
            return Token(TK_LESS_EQUAL, "", sc.row, beganTokenAtCol);
        }else{
            return Token(TK_LESS, "", sc.row, beganTokenAtCol);
        }
    }
    if(sc.match('-')) return Token(TK_MINUS, "", sc.row, beganTokenAtCol);
    if(sc.match('+')) return Token(TK_PLUS, "", sc.row, beganTokenAtCol);
    if(sc.match(';')) return Token(TK_SEMICOLON, "", sc.row, beganTokenAtCol);
    if(sc.match('/')) return Token(TK_SLASH, "", sc.row, beganTokenAtCol);
    if(sc.match('*')) return Token(TK_STAR, "", sc.row, beganTokenAtCol);
    if(sc.match('{')) return Token(TK_LBRACE, "", sc.row, beganTokenAtCol);
    if(sc.match('(')) return Token(TK_LPAREN, "", sc.row, beganTokenAtCol);
    if(sc.match('}')) return Token(TK_RBRACE, "", sc.row, beganTokenAtCol);
    if(sc.match(')')) return Token(TK_RPAREN, "", sc.row, beganTokenAtCol);
    if(sc.match(',')) return Token(TK_COMMA, "", sc.row, beganTokenAtCol);
    if(sc.match('[')) return Token(TK_L_SQ_BRACKET, "", sc.row, beganTokenAtCol);
    if(sc.match(']')) return Token(TK_R_SQ_BRACKET, "", sc.row, beganTokenAtCol); 

    //no operator matched
    return std::nullopt;

}

std::optional<char> next_token_is_digit(Scanner &sc){
    std::array<char, 10> digits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}; //char digits[]
    for(auto i: digits){ //s std array zase muzu toto :)
        if(sc.match(i)){
            return i;
        }
    }
    return std::nullopt;
}

//mozna by se melo jmenovat number token (digit je cislice)
std::optional<Token> match_digit_token(Scanner &sc){
    int beganTokenAtCol = sc.column;
    auto foundDigit = next_token_is_digit(sc);
    if(!foundDigit.has_value()){
        return std::nullopt;
    }
    std::string numberString;
    while(foundDigit.has_value()){
        if(foundDigit.has_value()){
            numberString += foundDigit.value();
        }
        foundDigit = next_token_is_digit(sc);
    }
    return Token(TK_NUMBER, numberString, sc.row, beganTokenAtCol);
}

//na promenne, podporuje ascii
//Konkrétně nepřerušované sekvence malých a velkých písmen abecedy nebo číslic, které nezačínají číslicí.
// => tedy '_' nebere jako validni znak
std::optional<Token> match_keyword_or_name_token(Scanner &sc){
    int beganTokenAtCol = sc.column;
    //prvni znak nesmi byt cislo
    if(next_token_is_digit(sc).has_value()){
        return std::nullopt;
    }
    //dalsi znaky muzou jak znaky v abecede tak cisla
    std::string keyword_or_name = "";
    while(std::isalpha(sc.peek()) || std::isdigit(sc.peek())){
        //mohl bych napsat funkci se smyckou pro isalpha, ktera by volala match, (podobne jako next_token_is_digit)
        //ale takhle se mi to zda cistsi
        keyword_or_name += sc.peek();
        sc.advance();
    }
    //v Pythonu bych na to pouzil dictionary (keyword: TokenType)
    //ale tady unordered_map ? asi lepsi nez if chain neni
    if(keyword_or_name == "else"){
        return Token(TK_ELSE, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "for"){
        return Token(TK_FOR, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "if"){
        return Token(TK_IF, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "print"){
        return Token(TK_PRINT, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "var"){
        return Token(TK_VAR, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "while"){
        return Token(TK_WHILE, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "&&"){
      return Token(TK_AND, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "||"){
      return Token(TK_OR, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "fn"){
      return Token(TK_FN, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == "return"){
      return Token(TK_RETURN, "", sc.row, beganTokenAtCol);
    }
    if(keyword_or_name == ""){
        return std::nullopt;
    }
    return Token(TK_NAME, keyword_or_name, sc.row, beganTokenAtCol);
}

std::optional<Token> match_string_literal(Scanner &sc){
  if(sc.peek() != '"'){
    return std::nullopt;
  }
  int beginColumn = sc.column;
  sc.advance();
  std::string result;
  while(!sc.is_at_end() && sc.peek() != '"'){
    result += sc.peek();
    sc.advance();
  }
  if(sc.peek() != '"'){
    std::cout << "Po uvozovce musí být někde uvozovka!\n";
    std::exit(1);
  }
  sc.advance();
  return Token(TK_STRING, result, sc.row, beginColumn);
}

std::optional<Token> lex_one(Scanner &sc) {
  // přeskoč whitespace
  while (std::isspace(sc.peek())) {
    sc.advance();
  }
  std::optional<Token> foundStringLiteral = match_string_literal(sc);
  if(foundStringLiteral.has_value()){
    return foundStringLiteral;
  }
  std::optional<Token> foundOperator = match_operator_token(sc);
  if(foundOperator.has_value()){
    return foundOperator;
  }
  std::optional<Token> foundNumber = match_digit_token(sc);
  if(foundNumber.has_value()){
    return foundNumber;
  }

  std::optional<Token> foundKeywordOrVariable = match_keyword_or_name_token(sc);
  if(foundKeywordOrVariable.has_value()){
    return foundKeywordOrVariable;
  }

  // jinak jsme museli narazit na konec nebo je zde nerozponany token
  if(!sc.is_at_end()){
    std::cerr << "chybny token at " << sc.row << ", " << sc.column << " (zvyrazneno cervene)\n";
    std::cerr << "radka " << sc.row << ": " <<  decoratedLine(sc.originalInput, sc.row, sc.column) << "\n";
  }
  return std::nullopt;    
}

std::vector<Token> lex(std::string source) {
  Scanner sc = Scanner(source);
  std::vector<Token> ts;

  while (true) {
    auto t = lex_one(sc);
    if (!t.has_value()) { break; }
    ts.push_back(t.value());
  }

//   ts.push_back(TK_EOF); //to jsem sem přidal já, nemyslím si, že je potřeba

  return ts;
}
