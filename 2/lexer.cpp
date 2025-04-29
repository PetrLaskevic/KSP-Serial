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

  // Klíčová slova
  TK_ELSE, TK_FOR, TK_IF, TK_PRINT, TK_VAR,
  TK_WHILE,

  // literály
  TK_NAME, TK_NUMBER,
};


struct Token {
  TokenType type;

  // pouze u TK_NAME a TK_NUMBER, jinak ""
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
    case  TK_ELSE: return "ELSE";
    case  TK_FOR: return "FOR";
    case  TK_IF: return "IF";
    case  TK_PRINT: return "PRINT";
    case  TK_VAR: return "VAR";
    case  TK_WHILE: return "WHILE";
    case  TK_NAME: return "NAME";
    case  TK_NUMBER: return "NUMBER";
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
    if(keyword_or_name == ""){
        return std::nullopt;
    }
    return Token(TK_NAME, keyword_or_name, sc.row, beganTokenAtCol);
}

std::optional<Token> lex_one(Scanner &sc) {
  // přeskoč whitespace
  while (std::isspace(sc.peek())) {
    sc.advance();
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

  return ts;
}

int main() {
  std::string source = "var zcelaSkvelyNazev123a = 369+21;\nif( !neco == 3){\nfunkce()\n}\nif skvelaPromenna2  + neco == 3:"; //"\ntest ifelse if var ~  invalid_variableName_ = 369+2-1\nskvelaPromenna2 neco|| == 3" //"var a  =  33+2;" //"var skvelaPromenna2 = 369+2-1\nskvelaPromenna2 neco|| == 3"
  std::vector<Token> ts = lex(source);

  std::cout << "\ncelkove nalexovano:\n";
  for (auto t : ts) {
    //radky a sloupce indexuju, jako spravny programator, od 0 :D
    if(t.value == ""){ 
        //            znak pro zelenou barvu
        std::cout << "\u001b[32m" << token_type_to_str(t.type) << "\u001b[0m" << " at: " << t.row << ", " << t.column << ' ' << '\n';
    }else{
        std::cout << "\u001b[32m" << token_type_to_str(t.type) << "("
                  << t.value << ") " << "\u001b[0m" << " at: " << t.row << ", " << t.column << ' ' << '\n';
    }
  }
  std::cout << '\n';
}