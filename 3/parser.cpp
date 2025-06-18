

#include "token.hpp"
#include "stack.hpp"
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
  ET_NAME, //promenna pouzita nekde print(var)
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
  ET_VAR, //pro deklaraci promenne
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

  //list našeho stromu, deklarace proměnné
  if(ts.match(TK_VAR)){
    if(ts.check(TK_NAME)){
      std::string varName = ts.peek().value;
      ts.advance();
      return Expr(ET_VAR, varName);
    }
    ts.error("No variable name detected after var");
  }
  //list našeho stromu, použítí proměnné někde místo čísla ("dosazení")
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

  while(ts.check(TK_PLUS) || ts.check(TK_MINUS)){
    Token operation = ts.peek();
    ts.advance();
    Expr right = multiplication(ts);
    ExprType type = (operation.type == TK_PLUS)
                    ? ET_ADD
                    : ET_SUBTRACT;
    left = Expr(type, {left, right});
  }
  //pokud pravý podstrom není, tak return jenom levý
  return left;
}
bool isEqualityOp(TokenType t){
  switch(t){
    case TK_GREATER: return true;
    case TK_LESS: return true;
    case TK_EQUAL_EQUAL: return true;
    case TK_NOT_EQUAL: return true;
    case TK_GREATER_EQUAL: return true;
    case TK_LESS_EQUAL: return true;
    default: return false;
  }
}

ExprType TKtoExprType(TokenType t){
  switch(t){
    case TK_GREATER: return ET_GREATER;
    case TK_LESS: return ET_LESS;
    case TK_EQUAL_EQUAL: return ET_EQUAL;
    case TK_NOT_EQUAL: return ET_NOT_EQUAL;
    case TK_GREATER_EQUAL: return ET_GREATER_EQUAL;
    case TK_LESS_EQUAL: return ET_LESS_EQUAL;
    default: {
      std::cerr << "Ani jeden typ nepasoval!\n";
      std::exit(1);
    };
  }
}

Expr comparison(TokenScanner &ts) {
  Expr left = addition(ts);
  TokenType nextToken = ts.peek().type;
  //je porovnavaci operator?
  //replaces the plethora of ts.match checks
  if(isEqualityOp(nextToken)){
    ts.advance();
    Expr right = addition(ts);
    /*VYSVĚTLENÍ ŘEŠENÍ CHAINOVÁNÍ OPERÁTORŮ POROVNÁVÁNÍ:
      TLDR: zvolil jsem variantu explicitního uzávorkování
      (která mi spolu s python variantou jediná dává smysl - 
      C chování se snadno stává shotgun :D) 
    Implementačně:
      už jak to bylo předtím jenom s == operátorem to by accident nebyl problém,
      při vstupu a == b == c (neplatném, chci závorky (a == b) == c) 
      => shodí to ts.error("Unexpected token"); v primary() (do něj se statement začínající ==, na který nic nepasuje, probublá), 
      protože vyhodnocením a == b se tím ukončí jeden statement(), a na to, co ze seznamu tokenů zbyde
      (TK_EQUAL_EQUAL a NAME(c)), se spustí další statement(),
      kde to nebude na nic matchovat a probublá se to do primary, které hodí výjimku*/
    // přesto ale budu zdvořilejší, a řeknu užívateli, v čem je problém:
    if(isEqualityOp(ts.peek().type)){
      ts.error("Nepodporujeme řetězení porovnávacích operátorů bez explicitního či validního uzávorkování, chceme př. (a == b) == c"); //omg, ani se nerozbila diakritika
    }
    return Expr(TKtoExprType(nextToken), {left, right});
  }
  return left;
}

std::string prefixPrint(Expr& node);

Expr assignment(TokenScanner &ts) {
  // a == b = c by bylo docela crazy
  //ve finále comparison bude porovnávat čísla, co by mělo dělat true=c ?
  // => je to zajímavý side effect toho, jak je to napsaný
  // => pro intended použití přiřazení do proměnné se to tím comparisonem a dál prostě probublá 
  //    (protože nic nebude matchovat, tak to bude vracet left s outputem dalšího callu)
  //    až se to dostane do primary, kde se už vyhodnocují listy (třeba literals, a použítí proměnné někde (var + 3)
  //=> takže TLDR: comparison se provolá do primary, kde se naparsuje proměnná, to bude levá stana, ke které budeme přiřazovat
  //a tak dostaneme ASSIGN(VAR(neco), UN_MIN(2)) pro var neco = -2
  //                ASSIGN(VAR(neco), promenna)  pro var neco = promenna
  Expr left = comparison(ts);

  if (ts.match(TK_EQUAL)) {
    Expr right = assignment(ts);
    // var neco=-!0+25*3+3-5-1/6;
    std::cout << "ooooooooooooooooooooo\n";
    std::cout << prefixPrint(left) << "\n";
    std::cout << prefixPrint(right) << "\n";
    std::cout << "ooooooooooooooooooooo\n";
    return Expr(ET_ASSIGN, {left, right});
  }
  return left;
}

Expr expression(TokenScanner &ts) {
  Expr a = assignment(ts);
  // ts.consume(TK_SEMICOLON, "Expected ';' after expression.");
  if(!ts.match(TK_SEMICOLON)){
    ts.error("Expected ';' after expression. Parsed so far:\n" + prefixPrint(a) + "\n");
  }
  return a;
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

//vypise strom infixove, podporuje pouze aritmeticke vyrazy
std::string printExprTree(Expr& node){
  if(node.type == ET_LITERAL){
    return node.value;
  }
  if(node.type == ET_NAME){ //co je to - proměnná?
    return node.value;
  }
  if(node.type == ET_VAR){
    return "var " + node.value;
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

std::string allOPToString(Expr& node){
  ExprType t = node.type;
  switch (t) {
    // binární operátory
    case ET_MULTIPLY: return "MUL";
    case ET_DIVIDE: return "DIV";
    case ET_ADD: return "ADD";
    case ET_SUBTRACT: return "SUBST";
    case ET_LESS: return "LESS";
    case ET_LESS_EQUAL: return "LESS_EQ";
    case ET_GREATER: return "MORE";
    case ET_GREATER_EQUAL: return "MORE_EQ";
    case ET_EQUAL: return "EQ";
    case ET_NOT_EQUAL: return "NOT_EQ";
    case ET_ASSIGN: return "ASSIGN";
    case ET_NEGATE: return "UN_MIN"; //unary minus
    case ET_NOT: return "NOT";
    case ET_BLOCK: return "BLOCK";
    //nic není pravda
    default: return "";
  }
}
//vypise strom prefixove, jakoby vznikl sekvenci techto jakoby function callu na zasobniku
std::string prefixPrint(Expr& node){
  std::string op = allOPToString(node);
  //leaves
  if(node.type == ET_LITERAL || node.type == ET_NAME){
    return node.value;
  }
  if(node.type == ET_VAR){
    return "VAR(" + node.value + ")";
  }
  //any number of children: ET_BLOCK
  //expressions: 2 children (binary operators) or 1 child (unary operators)
  op = op + "(";
  std::string separator = ",";
  if(node.type == ET_BLOCK){
    op += "\n";
    separator = ";\n";
  }
  for(Expr child: node.children){
    op += " " + prefixPrint(child) + separator;
  }
  //remove the last ","
  op.pop_back();
  if(node.type == ET_BLOCK){
    op += "\n";
  }
  op += " )";
  return op;

  std::cerr << "unhandled node in prefixPrint\n";
}


void emit(std::vector<Instruction> &program,
          Expr &expr) {
  for (auto& operand : expr.children) {
    emit(program, operand);
  }

  switch (expr.type) {
    case ET_ADD: {
      program.push_back(Instruction{.op = OP_ADD});
    } break;
    case ET_MULTIPLY: {
      program.push_back(Instruction{.op = OP_MUL});
    } break;
    case ET_SUBTRACT: {
      program.push_back(Instruction{.op = OP_SUB});
    } break;
    // ...
    case ET_LITERAL: {
      // Tady konečně naparsujeme číslo ze stringu
      // :)
      int value = std::stoi(expr.value);
      program.push_back(Instruction{
          .op = OP_PUSH, .value = value});
    } break;
  }
}

int main(){
  // "3-5+2" //"-1+1+2+1-2-3" var neco=3;neco=5;
  // "var neco = 3" => BLOCK(VAR(neco), ASSIGN(neco, 3))
  // "(a <= (b != c)) == d;" => BLOCK(EQ(LESS_EQ(a, NOT_EQ(b, c)), d))
  // "var a = 1 + 2 * 9 / -3;var b = 0;" => BLOCK(ASSIGN(VAR(a), ADD(1, DIV(MUL(2, 9), UN_MIN(3)))), ASSIGN(VAR(b), 0))
  // "var neco=-!0-1/6;" "var neco=0-12/6;"
  // "10-12+6" => BLOCK( ADD( SUBST( 10, 12), 6))
  std::string source = "a = -!0+25*3+3-5+-1/6;a = a -1;c=10-12+6;"; //"-!0+25*3+3-5+-1/6" //"var zcelaSkvelyNazev123a = 369+21;\nif( !neco == 3){\nfunkce()\n}\nif skvelaPromenna2  + neco == 3:"; //"\ntest ifelse if var ~  invalid_variableName_ = 369+2-1\nskvelaPromenna2 neco|| == 3" //"var a  =  33+2;" //"var skvelaPromenna2 = 369+2-1\nskvelaPromenna2 neco|| == 3"
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
  // std::cout << tokenScanner.isAtEnd() << "\n";
  // auto ast = expression(tokenScanner);
  // std::cout << printExprTree(ast) << "\n";
  auto ast = parse(tokenScanner);
  std::cout << "PUVODNI___________ " << source << "_________\n";
  std::cout << printExprTree(ast) << "\n";
  std::cout << prefixPrint(ast) << "\n";
}