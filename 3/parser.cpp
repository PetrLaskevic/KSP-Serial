

#include "token.hpp"
#include <cctype>
#include <vector>

struct TokenScanner {
  std::vector<Token> source;

  TokenScanner(std::vector<Token> s)
      : source{s} {}

  bool isAtEnd() { return source.empty(); }

  Token peek() {
    return source.empty() ? Token(TK_EOF)
                          : source[0];
  }

  //posun o 1 (nedělá žádné kontroly)
  void advance() {
    //mozna pridat kontrolu, jesli neni prazdny?
    if(source.size() == 0){
      std::cerr << "prazdny seznam tokenu, ale snazime se mazat!\n";
    }else{
      //dokud jsem nepridal kontrolu, tak to delalo segfault xd
      //mel jsem chybu v Expr addition, ktera mazala dalsi token (=call advance), kdyz dalsi token nebyl +
      source.erase(source.begin());
    }
  }
  //nedeskruktivní kontrola jestli odpovídá typ dalšího tokenu očekáváním
  bool check(TokenType type) {
    if (isAtEnd()) return false;
    return peek().type == type;
  }

  bool match(TokenType type) {
    return match(std::vector<TokenType>{type});
  }

  //O krok dál (smazat aktuální token) pokud odpovídá výčtu typů co dodáme
  bool match(std::vector<TokenType> types) {
    for (TokenType type : types) {
      if (check(type)) {
        advance();
        return true;
      }
    }
    return false;
  }

  //ta funkce co používáme pro "o krok dál" smazání tokenu, hází výjimku když nesedí typy
  void consume(TokenType type, const std::string &message) {
    if (!match(type)) {
      error(message);
    }
  }

  void error(const std::string &message) {
    if (isAtEnd()) {
      std::cerr << "Error at the end: ";
    } else {
      Token token = peek();
      std::cerr << "Error on token " <<
        token_type_to_str(token.type) <<
        "(" << token.value << ")" <<
        " at " << token.row <<
        ":" << token.column << ": ";
    }
    std::cerr << message << "\n";
    std::exit(1);
  }
};

// typy vrcholů AST
enum ExprType {
  ET_LITERAL,
  ET_NAME,
  // binární operátory
  ET_MULTIPLY,
  ET_DIVIDE,
  ET_ADD,
  ET_SUBTRACT,
  ET_LESS,
  ET_LESS_EQUAL,
  ET_GREATER,
  ET_GREATER_EQUAL,
  ET_EQUAL,
  ET_NOT_EQUAL,
  ET_ASSIGN,

  // unární operátory
  ET_NEGATE, //podle zdrojáku unary, s tokenem Minus proste -a = -1 * a //podle názvu by mohlo asi bitwise negation ~ nebo?
  ET_NOT, //klasická logická negace !true = false; a naopak

  // příkazy
  ET_BLOCK,
  ET_PRINT,
  ET_VAR,
  ET_IF,
  ET_WHILE,
  ET_FOR,
};

//vrchol AST
struct Expr {
  Expr(ExprType t, std::vector<Expr> ch)
      : type{t}, children{ch} {}

  Expr(ExprType t, std::string v)
      : type{t}, value{v} {}

  ExprType type;
  std::vector<Expr> children;
  std::string value; // pokud je number nebo name (tedy list I guess)
};


Expr expression(TokenScanner &ts);
Expr unary(TokenScanner &ts);

Expr printStatement(TokenScanner &ts) {
  Expr value = expression(ts);
  ts.consume(TK_SEMICOLON,
             "Expected ';' after value.");
  return Expr(ET_PRINT, {value});
}

Expr statement(TokenScanner &ts) {
  if (ts.match(TK_PRINT)){
    return printStatement(ts);
  }
  return expression(ts);
}

Expr parse(TokenScanner &ts) {
  std::vector<Expr> statements;

  while (!ts.isAtEnd()) {
    statements.push_back(statement(ts));
  }

  return Expr(ET_BLOCK, statements);
}

Expr primary(TokenScanner &ts) {
  auto token = ts.peek();

  //list našeho stromu
  if (ts.match(TK_NUMBER)) {
    return Expr(ET_LITERAL, token.value);
  }

  //list našeho stromu
  if (ts.match(TK_NAME)) {
    return Expr(ET_NAME, token.value);
  }

  if (ts.match(TK_LPAREN)) {
    // návrat zpět na začátek řetězce funkcí
    Expr e = expression(ts);
    ts.consume(TK_RPAREN, "Expected ')'");
    return e;
  }

  ts.error("Unexpected token");
  return Expr(ET_LITERAL, "0"); // unreachable, error() končí program že jo, abych se vyhnul "Warning: control reaches end of non-void function"
}

Expr unary(TokenScanner &ts) {
  // prostě -a (afaik, proč se to pak jmenuje negate) => možná protože negace ČÍSLA (a ne výroku)
  if (ts.match(TK_MINUS))
    //afaik v tomhle to znamená, že můžu chainit ---a
    //což třeba node.js nepodporuje
    return Expr(ET_NEGATE, {unary(ts)});
  // !a
  else if (ts.match(TK_NOT))
    //rekurzivní protože !!a můžu chainit za sebou
    return Expr(ET_NOT, {unary(ts)});
  else
    return primary(ts);
}

Expr multiplication(TokenScanner &ts) {
  Expr expr = unary(ts);

  while (ts.check(TK_STAR) || ts.check(TK_SLASH)) {
    Token op = ts.peek(); //uložíme si další token
    ts.advance(); //smažeme ho ze seznamu neprojitých tokenů
    //volání unary místo expression neudělá další call do multiplication
    // => ale zároveň zpracuje unární operátory, které "mají přednost"
    //tam se ta expansion může dít, !!!!!!!1 je naprosto validní
    Expr right = unary(ts);
    ExprType type = (op.type == TK_STAR)
                        ? ET_MULTIPLY
                        : ET_DIVIDE;
    expr = Expr(type, {expr, right});
  }

  return expr;
}

Expr addition(TokenScanner &ts) {
  std::cout << "Dostali jsme se sem\n";
  Expr left = multiplication(ts);
  //mělo by být ekvivalentní ts.match(TK_PLUS)
  if(ts.check(TK_PLUS)){
    ts.advance();
    Expr right = addition(ts);
    return Expr(ET_ADD, {left, right});
  }
  //tohle právě nemůžeme udělat, protože nám z vstupu 1 - 2 - 3 udělá 1 - (2-3)
  // => tady podobně jako u dělení musíme udělat zleva asociativní
  // (násobení a sčítání nemuselo = to je i zleva i zprava)
  // if(ts.check(TK_MINUS)){
  //   ts.advance();
  //   Expr right = addition(ts);
  //   return Expr(ET_SUBTRACT, {left, right});
  // }
  while(ts.check(TK_MINUS)) {
    //požrání operátoru
    ts.advance();
    Expr right = unary(ts);
    //leva asociativita se dělá tímhle
    //vezme se dosavadní všechno, dá se to do levého syna, 
    //pravý syn se udělá unary (aby se něco zpracovalo, ale nedošlo k dalšímu callu addition a expansi - + = tím by vznikla pravá asociativita)
    //pak se udělá nový kořen tohodle podstromum, s operací substract, s tímhle levým synem (předchozí všechno podstrom) a pravým synem dalším prostě **číslem** (akorát s !! -- zpracováním)
    //a přiřadí se to do levého podstromu dalšího kroku této operace
    //takže cyklus může pokračovat
    //protože unary sežere jedno číslo a neeexpanduje donekonečna, tak je tady právě ten while(ts.check(TK_MINUS),
    //který zajišťuje, že parsování tokenů do stromu neskončí
    left = Expr(ET_SUBTRACT, {left, right});
  }
  //pokud pravý podstrom není, tak return jenom levý
  return left;
}

Expr comparison(TokenScanner &ts) {
  Expr left = addition(ts);
  if(ts.match(TK_EQUAL_EQUAL)){
    Expr right = addition(ts);
    return Expr(ET_EQUAL, {left, right});
  }
  return left;
}

Expr assignment(TokenScanner &ts) {
  // a == b = c by bylo docela crazy
  //ve finále comparison bude porovnávat čísla, co by mělo dělat true=c ?
  Expr left = comparison(ts);

  if (ts.match(TK_EQUAL)) {
    Expr right = assignment(ts);
    return Expr(ET_ASSIGN, {left, right});
  }

  return left;
}

Expr expression(TokenScanner &ts) {
  return assignment(ts);
}
std::string binaryExprOperatorToString(Expr& node){
  ExprType t = node.type;
  switch (t) {
    // binární operátory
    case ET_MULTIPLY: return "*";
    case ET_DIVIDE: return "/";
    case ET_ADD: return "+";
    case ET_SUBTRACT: return "-";
    case ET_LESS: return "<";
    case ET_LESS_EQUAL: return "<=";
    case ET_GREATER: return ">";
    case ET_GREATER_EQUAL: return ">=";
    case ET_EQUAL: return "==";
    case ET_NOT_EQUAL: return "!=";
    case ET_ASSIGN: return "=";
    //nic není pravda
    default: return "";
  }
}

std::string unaryExprOperatorToString(Expr& node){
  ExprType t = node.type;
  switch (t) {
    //unární operátory
    case ET_NEGATE: return "-";
    case ET_NOT: return "!";
    //nic není pravda
    default: return "";
  }
}

std::string printExprTree(Expr& node){
  if(node.type == ET_LITERAL){
    return node.value;
  }
  if(node.type == ET_NAME){ //co je to - proměnná?
    return node.value;
  }
  //TODO: přidat to string příkazy

  bool isBinaryOP = (binaryExprOperatorToString(node) != "");
  if(isBinaryOP){
    std::cout << "binary operator " << binaryExprOperatorToString(node) << "\n";
    return "(" + printExprTree(node.children[0]) + binaryExprOperatorToString(node) + printExprTree(node.children[1]) + ")"; 
  }
  bool isUnaryOP = (unaryExprOperatorToString(node) != "");
  if(isUnaryOP){
    std::cout << "unary operator " << unaryExprOperatorToString(node) << "\n";
    return "[" + unaryExprOperatorToString(node) + printExprTree(node.children[0]) + "]";
  }
  if(node.type == ET_BLOCK){
    return printExprTree(node.children[0]);
  }
  return "UNHANDLED TOKEN";
}

int main(){
  // "3-5+2"
  std::string source = "1-2-3"; //"-!0+25*3+3-5+-1/6" //"var zcelaSkvelyNazev123a = 369+21;\nif( !neco == 3){\nfunkce()\n}\nif skvelaPromenna2  + neco == 3:"; //"\ntest ifelse if var ~  invalid_variableName_ = 369+2-1\nskvelaPromenna2 neco|| == 3" //"var a  =  33+2;" //"var skvelaPromenna2 = 369+2-1\nskvelaPromenna2 neco|| == 3"
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

  TokenScanner tokenScanner = TokenScanner(ts);
  std::cout << tokenScanner.isAtEnd() << "\n";
  // auto ast = expression(tokenScanner);
  // std::cout << printExprTree(ast) << "\n";
  auto ast = parse(tokenScanner);
  std::cout << printExprTree(ast) << "\n";
}