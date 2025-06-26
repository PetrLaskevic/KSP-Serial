

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
std::string prefixPrint(Expr& node, int identLevel = 1);

Expr printStatement(TokenScanner &ts) {
  Expr value = expression(ts);
  std::cout << "printStatement called\n";
  //v statement teď je early return na printStatement, tak ; check dám i sem
   if(!ts.match(TK_SEMICOLON)){
    ts.error("Expected ';' after expression. Parsed so far:\n" + prefixPrint(value) + "\n");
  }
  return Expr(ET_PRINT, {value});
}

Expr statement(TokenScanner &ts);
bool isEqualityExpr(ExprType t);
Expr block(TokenScanner &ts);
Expr assignment(TokenScanner &ts);

Expr if_statement(TokenScanner &ts) {
  ts.consume(TK_LPAREN, "expected '(' after 'if'");
  //tady pocitam klasicke if(boolean) nebo if(expression)
  //obojí by měl vyřešit expression, if(a) asi vyřeším na > 0 
  Expr ifCond = expression(ts);
  //pokud je výraz, tak ho nechám, pokud není př if(a), tak ho převedu na if(a>0)
  if(!isEqualityExpr(ifCond.type)){
    ifCond = Expr(ET_GREATER, {ifCond, Expr(ET_LITERAL, "0")});
  }
  ts.consume(TK_RPAREN,
            "expected ')' after an "
            "expression inside an if statement");

  //statement nove detekuje, jestli dalsi token je { a kdyz ano, tak vola block
  Expr ifBody = statement(ts);

  //statement volá block(), který bere tokeny dokud nenarazí na } kterou sežere
  //if(cond){
  //}else{}

  Expr elseBody(ET_BLOCK, std::vector<Expr>{});

  if(ts.match(TK_ELSE)){
    //pokud za else bude {, tak to bude dál block
    //pokud ne, tak se elseBody změní na nějaký expression co bude dál else print a;
    elseBody = statement(ts);
  }
  return Expr(ET_IF, {ifCond, ifBody, elseBody});
}

Expr while_statement(TokenScanner &ts){
  ts.consume(TK_LPAREN, "expected '(' after 'while'");
  //tady pocitam klasicke while(boolean) nebo while(expression)
  //obojí by měl vyřešit expression, while(a) asi vyřeším na a > 0 
  Expr condition = expression(ts);
  //pokud je výraz, tak ho nechám, pokud není př while(a), tak ho převedu na while(a>0)
  if(!isEqualityExpr(condition.type)){
    condition = Expr(ET_GREATER, {condition, Expr(ET_LITERAL, "0")});
  }
  ts.consume(TK_RPAREN,
            "expected ')' after an "
            "expression inside a while statement");

  //statement nove detekuje, jestli dalsi token je { a kdyz ano, tak vola block
  Expr body = statement(ts);
  return Expr(ET_WHILE, {condition, body});
}

// for smyčky v AST jako čtveřici: inicializační výraz, podmínkový výraz, výraz i = i + 1 a samotné tělo smyčky.
Expr for_statement(TokenScanner &ts) {
  ts.consume(TK_LPAREN, "expected '(' after 'for'");

  Expr init(ET_BLOCK, std::vector<Expr>{});
  if (ts.match(TK_SEMICOLON)) {
    // prázdný
  } else if (ts.check(TK_VAR)) { //check(), not match() => match ate the token which said it was a variable declaration => making the AST be like assignment to an existing variable
    // definice proměnné (středník kontroluje sám)
    init = assignment(ts); //v templatu var_statement, které neexistuje
    //možná ten šabloní var_statement ano, ale můj assignment ne, ; řeším nahoře v statement
    //proto si ; tady zkontroluju
    ts.consume(TK_SEMICOLON,
            "expected ';' after the first "
            "expression (assignment) inside a for statement");
  } else {
    //afaik pokud proměnná už je definována, tak s ní tam může být nějaký výraz (třeba assignment vlastně)
    //var a;
    //for(a = 3; atd..)
    init = expression(ts);
    ts.consume(TK_SEMICOLON,
               "expected ';' after the first "
               "expression inside a for statement");
  }

  //ten prostřední člen for statementu, ta podmínka
  //ternary kontroluje, jestli to není for(; ;) případ
  Expr cond = ts.check(TK_SEMICOLON)
                  ? Expr(ET_LITERAL, "1")
                  : expression(ts);

  ts.consume(TK_SEMICOLON,
             "expected ';' after the second "
             "expression inside a for statement");

  //třetí člen, i=i+1;
  Expr expr = ts.check(TK_RPAREN)
      ? Expr(ET_BLOCK, std::vector<Expr>{})
      : expression(ts);
  ts.consume(TK_RPAREN,
             "expected ')' after the third "
             "expression inside a for statement");

  /*co se provede uvnitř 
  for(){
    tady
  }
  */
  //pro for (var i = 0; i < 10; i = i + 1) print i; 
  // Expr body = statement(ts);
  //ale protože chceme povolit i víc statementů za sebou, tedy block
  //on se block bude nove volat ze stamentu, protoze v 
  //statementu je `if (ts.match(TK_LBRACE)) return block(ts);`
  //tedy handling {
  Expr body = statement(ts);

  return Expr(ET_FOR, {init, cond, expr, body});
}

Expr statement(TokenScanner &ts) {
  if (ts.match(TK_PRINT)) return printStatement(ts);
  // if (ts.match(TK_VAR)) return var_statement(ts); //mám v rámci assignment v expression, snad v pohodě
  if (ts.match(TK_FOR)) return for_statement(ts);
  if (ts.match(TK_IF)) return if_statement(ts);
  if (ts.match(TK_WHILE)) return while_statement(ts);

  //pokud { tak nový block
  if (ts.match(TK_LBRACE)) return block(ts);
  
  Expr a = expression(ts);
  //semicolon check na jednom miste, at uz je to call do expression nebo print statement
  if(!ts.match(TK_SEMICOLON)){
    ts.error("Expected ';' after expression. Parsed so far:\n" + prefixPrint(a) + "\n");
  }
  return a;
}

//předtím název parse, teď block
Expr block(TokenScanner &ts) {
  std::vector<Expr> statements;

  //dokud není } (tím blok končí) příp. dokud není konec souboru
  while (!ts.match(TK_RBRACE) && !ts.isAtEnd()) {
    statements.push_back(statement(ts));
  }
  //i block statement nově bude vracet
  statements.push_back(Expr(ET_LITERAL, "0"));
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

bool isEqualityExpr(ExprType t){
  switch(t){
    case ET_GREATER: return true;
    case ET_LESS: return true;
    case ET_EQUAL: return true;
    case ET_GREATER_EQUAL: return true;
    case ET_LESS_EQUAL: return true;
    case ET_NOT_EQUAL: return true;
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

  //asociativní zprava
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
  //expressions can be nested (TK_LPAREN causes an expression call)
  //so, the semicolon check was put one level up, in statement
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
    //to avoid segmentation fault on empty blocks
    if(node.children.size() == 0){
      return "EMPTY BLOCK";
    }
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
    case ET_PRINT: return "PRINT";
    case ET_VAR: return "VAR_DECL";
    case ET_LITERAL: return "LITERAL";
    case ET_NAME: return "VAR_USE";
    case ET_IF: return "IF";
    case ET_WHILE: return "WHILE";
    case ET_FOR: return "FOR";
    //nic není pravda
    default: return "";
  }
}
//vypise strom prefixove, jakoby vznikl sekvenci techto jakoby function callu na zasobniku
std::string prefixPrint(Expr& node, int identLevel){ //int identLevel optional, coz definuju nahore std::string prefixPrint(Expr& node, int identLevel = 1);
  std::string op = allOPToString(node);
  //leaves
  if(node.type == ET_LITERAL){
    return "LIT(" + node.value + ")";
  }
  if(node.type == ET_NAME){
    return "NAM(" + node.value + ")";
  }
  if(node.type == ET_VAR){
    return "VAR(" + node.value + ")";
  }
  //any number of children: ET_BLOCK
  //expressions: 2 children (binary operators) or 1 child (unary operators)
  op = op + "(";
  std::string separator = ",";
  std::string ident(identLevel*2, ' ');
  if(node.type == ET_BLOCK){
    op += "\n";
    separator = ";\n";
    identLevel++;
  }
  std::string usedIdent = " ";

  if(node.type == ET_IF){
    identLevel++;
    op += usedIdent + prefixPrint(node.children[0], identLevel) +
    "{\n" + ident + prefixPrint(node.children[1], identLevel) + "}";
    if(node.children[2].children.size() != 0){
      op += "ELSE{\n" + ident + prefixPrint(node.children[2], identLevel) + "}";
    }
  }else{
    for(Expr child: node.children){
      if(node.type == ET_BLOCK){
        usedIdent = ident;
      }
      op += usedIdent + prefixPrint(child, identLevel) + separator;
    }
  }
  //remove the last ","
  op.pop_back();
  if(node.type == ET_BLOCK){
    op += "\n";
  }
  usedIdent.pop_back();
  if(usedIdent.size() > 0){
    usedIdent.pop_back();
  }
  op += usedIdent + ")";
  return op;

  std::cerr << "unhandled node in prefixPrint\n";
}

void emit_condition(std::vector<Instruction> &program,
                    Expr &condition, Expr &if_true,
                    Expr &if_false);

void emit_while(std::vector<Instruction> &program,
                Expr &condition, Expr &whileBody);

void emit_for(std::vector<Instruction> &program,
              Expr &init,
              Expr &condition,
              Expr &conditionChanger,
              Expr &body);

void emit(std::vector<Instruction> &program,
          Expr &expr) {

    /*for `ET_` cases which don't handle recursion to see the rest of the subtree in DFS order themselves
    (the other cases handle recursion inside themselves, calling emit inside)
    => duplicate calls which evaluate some subtrees twice of more

    problém je, že v případech, kdy výraz (expression) obsahuje unární mínus, nebo assignment,

    se ET_LITERAL uvnitř vykoná více než jednou (podle výrazu):
    př. pro výraz "var a = 9 / -3;" 4x nebo pro "-3" 2x

    => jsou to ty případy, které si pro podstrom samy volají emit, protože po výsledku podstromu - instrukcích
    potřebují něco dát na zásobník (a jinak to moc rozumně afaik nejde)*/
    if(
      expr.type != ET_NEGATE &&
      expr.type != ET_ASSIGN &&
      expr.type != ET_PRINT &&
      //rovnosti - potřebují vyhodnotit levou a pravou stranu a pak je porovnat
      expr.type != ET_GREATER &&
      expr.type != ET_GREATER_EQUAL &&
      expr.type != ET_LESS &&
      expr.type != ET_LESS_EQUAL &&
      expr.type != ET_EQUAL &&
      expr.type != ET_NOT_EQUAL &&
      expr.type != ET_IF &&
      expr.type != ET_WHILE &&
      expr.type != ET_FOR
    ){
      for (auto& operand : expr.children) {
        std::cout << "operand: " << allOPToString(operand) << "\n"; 
        emit(program, operand);
      }
    }

  switch (expr.type) {
    case ET_ADD: {
      program.push_back(Instruction{.op = OP_ADD});
    } break;
    case ET_MULTIPLY: {
      program.push_back(Instruction{.op = OP_MUL});
    } break;
    case ET_DIVIDE: {
      program.push_back(Instruction{.op = OP_DIV});
    } break;
    case ET_SUBTRACT: {
      program.push_back(Instruction{.op = OP_SUB});
    } break;
    case ET_NEGATE: {
      program.push_back(Instruction{.op = OP_PUSH, .value = 0});
      emit(program, expr.children[0]); //emit(operand) část
      program.push_back(Instruction{.op = OP_SUB});
    } break;
    case ET_PRINT: {
      //to už by chtělo celý další call na emit, 
      //protože v definici printStatement vzniknuvší 
      // program.push_back();
      Expr printedExpression = expr.children[0];
      //hopefully ten výraz dá něco na zásobník
      emit(program, printedExpression);
      //instrukce OP_PRINT z něj hodnotu vypíše, ale nesebere
      program.push_back(Instruction{.op = OP_PRINT});
      //tj, díky tomu emitu, kde je PUSH v na ET_LITERAL nebo ET_NAME
      //vrací i ET_PRINT hodnotu 
    } break;
    case ET_LITERAL: {
      // Tady konečně naparsujeme číslo ze stringu
      // :)
      int value = std::stoi(expr.value);
      std::cout << "LITERALLLLLL " << value << "\n";
      program.push_back(Instruction{
          .op = OP_PUSH, .value = value});
    } break;
    case ET_NAME: {
      std::cout << "NAME\n";
      //Expr(ET_NAME, token.value); má .value string token.value svého jména
      //tahle instrukce hledá proměnnou s tím jménem v slovníku proměnných 
      //a pak její hodnotu dosadí na zásobník
    
      //ne tohle neplní, že přiřazení vrací hodnotu, jenom umožňuje aby použití proměnné fungovalo třeba v b = 3 + a;
      //ET_ASSIGN má děti: ET_NAME a ET_LITERAL (nebo výraz)
      //a emit aby se to expandovalo volám jenom na to druhé dítě (pravou stranu)
      //takže se k tomu ET_NAME jako levé straně v přiřazení nikdy nedostanem
      //zároveň ofc, pokud ET_NAME bude napravo, tak musíme dát na stack hodnotu té proměnné
      //ET_ASSIGN b = a; zavolá emit na a, takže tady se to dá na stack, a v ET_ASSIGN instrukcích za tím emit outputem se to sebere a přiřadí do b
      //jenže specifikace chce, aby na stacku byla hodnota b 
      //TLDR: obojí potřeba 
      program.push_back(Instruction{.op = OP_LOAD, .value = expr.value});
    } break;
    case ET_VAR: {
      //tohle by handlovalo var a; = to by muselo mít nějakou default hodnotu
      // std::cerr << "Deklaraci bez inicializace (bez přiřazení default hodnoty) nechceme.\n Použijte var a = 3; místo var a;\n";
      //rekurzivně se tam emit dostane i když celý výraz je var a = 3;
      //DEFAULT HODNOTA BUDE 0
      program.push_back(Instruction{.op = OP_PUSH, .value=0});
      program.push_back(Instruction{.op = OP_STORE, .value = expr.value });
      //protože všechny přikazy budou nově něco vracet:
      program.push_back(Instruction{.op = OP_PUSH, .value=0});
    } break;
    case ET_ASSIGN: {
      std::cout << "ASSIGN\n";
      //tady je ten moment, kdy zamítneme levou stranu cokoliv než proměnnou (for now)
      Expr leftSide = expr.children[0];
      if(
        leftSide.type != ET_VAR && //var a = 3; přiřazení k deklaraci
        leftSide.type != ET_NAME //a = 6; přiřazení k již deklarované proměnné
      ){
        std::cerr << "Přiřadit umíme jen k proměnné!\n";
        return;
      }
      //teď když házíme výjimky na `a = 5` když předtím nebylo `var a` nebo `var a = 2`
      //tak musíme v assignmentu než zkusíme LOAD tak nejdřív proměnnou deklarovat
      if(leftSide.type == ET_VAR){
        emit(program, leftSide);
      }
      //leftSide Expr(ET_VAR, varName); má .value = varName = název proměnné
      //rightSide je výraz napravo od rovnítka, z čehož bude sekvence instrukcí
      //hopefully, když se vyhodnotí, tak na zásobníku bude hodnota
      Expr rightSide = expr.children[1];
      emit(program, rightSide);

      
      //tu proměnnou naopak definujeme, dáme ji do slovníku
      //pomocí OP_STORE, která ale čeká hodnotu ze zásobníku, která tam bude po vyhodnocení pravého výrazu
      //=> Bylo by fajn, kdyby přiřazení do nedefinované proměnné vyhodilo chybu (za běhu).
      //protože ale OP_STORE neví, jestli proměnná existuje, nebo ji vytváříme (OP_STORE voláme i z deklarace),
      //tak bychom buď mohli udělat další op code pro deklaraci a upravit OP_STORE ať vypisuje error, když neexistuje
      //a nebo upravit OP_LOAD (který v podobě z 1. dílu proměnnou vytvářel, když nebyla)
      //=> šlo by hodit error tam, protože OP_LOAD nemá žádný pořádný způsob, jak předat, že selhal do téhle vrstvy
      //(aniž bych musel zakazovat nějakou hodnotu co může být na zásobníku / zaváděl další "systémovou" proměnnou)
      //navíc "kam" => vždyť tady instukce vytváříme, a interpret běží až potom
      program.push_back(Instruction{.op = OP_LOAD, .value = leftSide.value});
      //když bude load úspěšný, tak ok, když ne, tak ukončí interpret()
      //=> pokud teda ještě program běží, tak LOAD přidal extra hodnotu na zásobník, kterou je třeba odebrat
      program.push_back(Instruction{.op = OP_POP});
      program.push_back(Instruction{.op = OP_STORE, .value = leftSide.value});
      //v zadání chtějí: Přiřazení do proměnné vrací přiřazenou hodnotu, tak ji na zásobník znovu přidám
      program.push_back(Instruction{.op = OP_LOAD, .value = leftSide.value});
    } break;
    case ET_EQUAL: {
      //vyhodnocení levé strany => výstup ideálně 1 hodnota na zásobníku
      Expr leftSide = expr.children[0];
      emit(program, leftSide);
      //vyhodnocení pravé strany => výstup ideálně 1 hodnota na zásobníku
      Expr rightSide = expr.children[1];
      emit(program, rightSide);

      program.push_back(Instruction{.op = OP_EQ});

    } break; /*break je důležitý, protože jinak bude fall through = následující case ET_NOT_EQUAL proběhne BEZ kontroly */
    case ET_NOT_EQUAL: {
      //vyhodnocení levé strany => výstup ideálně 1 hodnota na zásobníku
      Expr leftSide = expr.children[0];
      emit(program, leftSide);
      //vyhodnocení pravé strany => výstup ideálně 1 hodnota na zásobníku
      Expr rightSide = expr.children[1];
      emit(program, rightSide);

      program.push_back(Instruction{.op = OP_EQ});
      program.push_back(Instruction{.op = OP_NOT});
    } break;
    case ET_LESS: {
      //vyhodnocení levé strany => výstup ideálně 1 hodnota na zásobníku
      Expr leftSide = expr.children[0];
      emit(program, leftSide);
      //vyhodnocení pravé strany => výstup ideálně 1 hodnota na zásobníku
      Expr rightSide = expr.children[1];
      emit(program, rightSide);

      program.push_back(Instruction{.op = OP_LT});
    } break;
    case ET_GREATER_EQUAL: {
      //to stejné jako ET_LESS a pak na to OP_NOT
      //takže >=

      //vyhodnocení levé strany => výstup ideálně 1 hodnota na zásobníku
      Expr leftSide = expr.children[0];
      emit(program, leftSide);
      //vyhodnocení pravé strany => výstup ideálně 1 hodnota na zásobníku
      Expr rightSide = expr.children[1];
      emit(program, rightSide);
      program.push_back(Instruction{.op = OP_LT});
      program.push_back(Instruction{.op = OP_NOT});
    } break;
    case ET_GREATER: {
      //Problém je, že mám jenom instrukce OP_LT, OP_EQ a OP_NOT
      //a taky, že v rámci výrazu (expression), který řeším, můžu mít přiřazení do proměnné.
      //Tedy nemůžu říct a < b takže b > a, a prostě prohodit pořadí callů emitu
      //protože bych mohl mít (a = a+3) > a*2, kdy 1. musím vyhodnotit levou stranu a až pak pravou 
      //(kde už je nová hodnota z levé strany)
      //(myslel jsem si, že můj dřívejší zákaz řetězení (asociativity) logických operátorů mi pomůže, ale na tohle to nemá vliv)
      //=> Právě kvůli těm přiřazením proměnných (a v budoucnu function callům) je v zadání to 
          // "Zachovávejte pořadí vyhodnocení operandů zleva doprava. 
          //Pokud potřebujete argumenty v opačném pořadí, musíte je prohodit nějak jinak"
      // "Například můžete přiřadit do pomocných proměnných, jen pozor, aby jejich jména nekolidovala s něčím jiným."
        //=> na úrovni instrukcí, to by šlo, ale musel bych si zakázat buď dva názvy proměnných pro uživatele (vyhrazeno pro toto)
        //a nebo XOR swap s jednou
      //já bych radši nezakazoval uživateli žádnou proměnnou, ALE ASI TO BEZ TOHO NEJDE (min 1 bych v kombinaci se swapem potřeboval)
        //chtěl jsem provést:
          //1.  ET_LESS a pak na to dal OP_NOT, tedy >=
          //2. pak ověřit rovnost
      
      //chtěl jsem použít instrukci OP_DUP v kombinaci s mou OP_SWAP 
      //(povoleno Standou, moje instrukce, která prohodí 2 věci na zásobníku)
      //ALE ASI NEJDE:
      //chtěl jsem dostat: (A = levá, B = pravá)
      //A
      //B
      //A
      //B
      //kdy jednu dvojici by požralo >= a druhou !=, pak bych udělal && (to jde, OP_ADD a OP_EQ == 2)
      //jenže duplikací si vyrobím 2 stejné hodnoty za sebou, které pak swapovat nemá smysl
      //kdyby to byla duplikace posledních 2 hodnot, tak by to šlo, nebo swap nechávající horní hodnotu a swapující 2 pod tím

      //takže proměnné it is

      Expr leftSide = expr.children[0];
      emit(program, leftSide);
      program.push_back(Instruction{.op = OP_STORE, .value = "promenna_jejiz_jmeno_nelze_vyslovit"});
      //vyhodnocení pravé strany => výstup ideálně 1 hodnota na zásobníku
      Expr rightSide = expr.children[1];
      emit(program, rightSide);
      program.push_back(Instruction{.op = OP_STORE, .value = "promenna_jejiz_jmeno_nelze_vyslovit2"}); //:trollhroch: nemusím uživatele instruovat, ať se tomu jménu vyhne, _ v nzevech proměnných nepodporujeme (error při parsování)

      //stačilo by vlastně toto
      program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit2"});
      program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit"});
      program.push_back(Instruction{.op = OP_LT});

      //ale pro fun convoluted vec, co me napadlo jako prvni: 
      //takže teď už je mám v proměnných, takže můžu provést (a >= b) && (a != b) pro a > b
      // >=
      // program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit"});
      // program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit2"});
      // program.push_back(Instruction{.op = OP_LT});
      // program.push_back(Instruction{.op = OP_NOT});
      // // !=
      // program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit"});
      // program.push_back(Instruction{.op = OP_LOAD, .value = "promenna_jejiz_jmeno_nelze_vyslovit2"});
      // program.push_back(Instruction{.op = OP_EQ});
      // program.push_back(Instruction{.op = OP_NOT});
      // //teď mám výsledky, můžu &&
      // program.push_back(Instruction{.op = OP_ADD}); //1+1=2
      // program.push_back(Instruction{.op = OP_PUSH, .value = 2});
      // program.push_back(Instruction{.op = OP_EQ});
    } break;
    case ET_IF: {
      Expr condition = expr.children[0];
      Expr if_true = expr.children[1];
      Expr if_false = expr.children[2];
      emit_condition(program, condition, if_true, if_false);
      //ať if taky něco vrací
      program.push_back(Instruction{.op = OP_PUSH, .value = 0});
    } break;
    case ET_WHILE: {
      Expr condition = expr.children[0];
      Expr body = expr.children[1];
      emit_while(program, condition, body);
      //ať while taky něco vrací
      program.push_back(Instruction{.op = OP_PUSH, .value = 0});
    } break;
    case ET_FOR: {
      Expr init = expr.children[0];
      Expr condition = expr.children[1];
      Expr expression = expr.children[2];
      Expr body = expr.children[3];
      emit_for(program, init, condition, expression, body);
      //ať for taky něco vrací
      program.push_back(Instruction{.op = OP_PUSH, .value = 0});
    } break;
  } 
}

void emit_condition(std::vector<Instruction> &program,
                    Expr &condition, Expr &if_true,
                    Expr &if_false) {
  // generujeme něco takového
  // * condition
  // * OP_BRANCH [if_true]
  // * if_false
  // * OP_PUSH 1
  // * OP_BRANCH [end]
  // * if_true
  // * end:
  emit(program, condition);

  auto condition_ix = size(program);
  // * OP_BRANCH [if_true]
  program.push_back(Instruction{
      .op = OP_BRANCH}); // value přiřadíme později

  emit(program, if_false);

  program.push_back(
      Instruction{.op = OP_PUSH, .value = 1});
  auto endjump_ix = size(program);
  //ten OP_BRANCH [end]
  program.push_back(Instruction{.op = OP_BRANCH});

  // sem by měla skočit podmínka, pokud je true
  program[condition_ix].value = (int)ssize(program);
  emit(program, if_true);

  // sem skáče nepodmíněný skok po `else` bloku
  program[endjump_ix].value = (int)ssize(program);
}

void emit_while(std::vector<Instruction> &program,
                Expr &condition, Expr &whileBody){
  // * begin
  // *emit(condition)
  // * OP_BRANCH [loop_body_begin] //if true
  // * OP_PUSH 1 //pokud dojde sem, aby skočilo na konec unconditionally
  // * OP_BRANCH [end]
  // * loop_body_begin
  // * emit(whileBody)
  // * OP_PUSH 1 //pokud nepřekočilo na konec, tak skočíme na začátek pro ověření podmínky znova
  // * OP_BRANCH [begin]
  // * end
  int beginIP = size(program);
  emit(program, condition);
  int branchInstructionPtr = size(program);
  program.push_back(Instruction{
    .op = OP_BRANCH, .value = branchInstructionPtr + 3
  });
  //unconditional jump na end, pokud jsme se sem dostali = nebyla splněna podmínka
  program.push_back(Instruction{
    .op = OP_PUSH,
    .value = 1
  });
  int endJumpInstructionAdress = size(program);
  program.push_back(Instruction{
    .op = OP_BRANCH,
  }); // value přiřadíme později

  emit(program, whileBody);

  //unconditional jump na ověřování podmínky (další iterace) 
  program.push_back(Instruction{
    .op = OP_PUSH,
    .value = 1
  });
  program.push_back(Instruction{
    .op = OP_BRANCH,
    .value = beginIP
  });
  int endIP = size(program);
  //aby měl kam skočit pro end
  program.push_back(Instruction{
    .op = OP_NOP
  });
  program[endJumpInstructionAdress].value = endIP;
}

void emit_for(std::vector<Instruction> &program,
              Expr &init,
              Expr &condition,
              Expr &conditionChanger,
              Expr &body){
  // * emit(init)
  // * begin
  // *emit(condition)
  // * OP_BRANCH [loop_body_begin] //if true
  // * OP_PUSH 1 //pokud dojde sem, aby skočilo na konec unconditionally
  // * OP_BRANCH [end]
  // * loop_body_begin
  // * emit(body)
  // * emit(conditionChanger)
  // * OP_PUSH 1
  // * OP_BRANCH [begin]
  // * end

  //declare or assign to variable (first in for(;;) )
  
  emit(program, init);

  int beginIP = size(program);

  emit(program, condition);
  int branchInstructionPtr = size(program);
  program.push_back(Instruction{
    .op = OP_BRANCH, .value = branchInstructionPtr + 3
  });

  //unconditional jump na end, pokud jsme se sem dostali = nebyla splněna podmínka
  program.push_back(Instruction{
    .op = OP_PUSH,
    .value = 1
  });
  int endJumpInstructionAdress = size(program);
  program.push_back(Instruction{
    .op = OP_BRANCH,
  }); // value end přiřadíme později

  emit(program, body);

  emit(program, conditionChanger);

  //unconditional jump na ověření podmínky znova = pokud jsme se sem dostali = proběhlo jedno kolo smyčky
  program.push_back(Instruction{
    .op = OP_PUSH,
    .value = 1
  });
  program.push_back(Instruction{
    .op = OP_BRANCH,
    .value = beginIP
  });

  int endIP = size(program);
  program.push_back(Instruction{
    .op = OP_NOP
  });
  //tell the unconditional jump na end, kam má skočit, když není splněna podmínka
  program[endJumpInstructionAdress].value = endIP;
}

int main(){
  // "3-5+2" //"-1+1+2+1-2-3" var neco=3;neco=5;
  // "var neco = 3" => BLOCK(VAR(neco), ASSIGN(neco, 3))
  // "(a <= (b != c)) == d;" => BLOCK(EQ(LESS_EQ(a, NOT_EQ(b, c)), d))
  // "var a = 1 + 2 * 9 / -3;var b = 0;" => BLOCK(ASSIGN(VAR(a), ADD(1, DIV(MUL(2, 9), UN_MIN(3)))), ASSIGN(VAR(b), 0))
  // "var neco=-!0-1/6;" "var neco=0-12/6;"
  // "10-12+6" => BLOCK( ADD( SUBST( 10, 12), 6))
  //a = -!0+25*3+3-5+-1/6;a = a -1;c=10-12+6;
  std::string source = //"var a = 3;a=6;print a;";
  "var a = 5;" //this alone puts three things on the stack
  "print a;"
  "while ((a = a - 1) >= 0) {"
    "print a;"
  "}";
  // "print 65535;"
  // "for (var i = 10; i < 20; i = i + 1) {"
  //   "var delitelne2 = i / 2 == (i + 1) / 2;"
  //   "print delitelne2;"
  //   "if (delitelne2) {"
  //   "  print i;"
  // "}"
  //   //tohle je nested loop
  //   "print 64;"
  //   "var vysledek = 1;"
  //   "var faktorial = 12;"
  //   "while(faktorial){"
  //   "  vysledek = vysledek * faktorial;"
  //   "  faktorial = faktorial - 1;"
  //   "}"
  // "print vysledek;}";

    // "var a = 0;"
    // "for(a = 0; a < 8; a = a + 1){"
    // "print a;}";
    // "if(a == 2){"
    // "print 10;"
    // "}else{"
    // "print 20;}"

    // "a = 5;"
    // "while(a >= 0){"
    //   "print a;"
    //   "if(a == 3){"
    //     "print a * a;}"
    //   "a = a - 1;}"
    // "print 255;"
    // "print a;";

    // "var a;"
    // "var b;"
    // "if (a > b) {"
    //   "while (b < a)"
    //     "print b;"

    //   "if (123)"
    //     "if (321)"
    //       "while (231)"
    //         "print 213;"
    // "}else{print 2;}";

    // "var a = 1 + 2 * 9 / -3;"
    // "print a;" //-5
    // "var b = 0;"
    // "print a;" //0
    // "print ((a = 10) * (b = 4)) / a / b;" //40/10/4 == 1
    // "print (a = 0) >= a;" //1
    // "print !(b > (b = 0));"; //1
    //po tomhle referencčím programzú dokonce 7

    //po tomhle na stacku ulozene zustavaji 2 osirele veci
    // "var promenna = -99*-2;"
    // "print promenna;";


    // "c = 5;"
    // "print c;";
   
  // "var a = 0;"
  // "-(a = a + 1);"
  // "print a;";
  // "var a = -5;"
  // "var b = a - 1;"
  // // "(a = 10) == a;"
  // // "print (a = 10) == a;"
  // "print a == 3;"
  // "print a != 3;"
  // "print a > -8;"
  // "print a > -5;"
  // "print a > a;"
  // "print a > 3;"
  // "print a > b;"
  // "print(a);"
  // //pro grafy f1(x): y = x+3 a f2(x): y = 2(x+3) platí f1>f2 od (-inf, -3)
  // "a = -2;" //2
  // "print((a = a+3) > a*2);"
  // //grafy f1(x): y = 2x a f2(x): y = x+3 platí f1<f2 pro od (-inf, +3)
  // //sjednocení, kdy platí oobjí je -4 a míň
  // //naopak kdy se výsledky liší je (-3, 3)
  // // v (-inf, -3) platí obojí a v (3, inf) neplatí nic
  // //takže -2 -1 0 1 2
  // "a = -2;" //2
  // "print((a*2) < (a = a+3));";
  // "print a == 3;";
  // "var a = 1 + 2 * 9 / -3;" //;print a;;"; //"-!0+25*3+3-5+-1/6" //"var zcelaSkvelyNazev123a = 369+21;\nif( !neco == 3){\nfunkce()\n}\nif skvelaPromenna2  + neco == 3:"; //"\ntest ifelse if var ~  invalid_variableName_ = 369+2-1\nskvelaPromenna2 neco|| == 3" //"var a  =  33+2;" //"var skvelaPromenna2 = 369+2-1\nskvelaPromenna2 neco|| == 3"
  // "print a;"
  // "var b = 0;"
  // //prostě dát tu kontrolu středníku jinam
  // "print ((a = 10) * (b = 4)) / a / b;" //tahle řádka ukazuje, že není dobrý chtít za každým expressionem ; => Error on token RPAREN() at 0:55: Expected ';' after expression. Parsed so far: => ASSIGN( LIT(a), LIT(10) )
  // "print (a = 0) >= a;"
  // "print !(b > (b = 0));";

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
  auto ast = block(tokenScanner);
  std::cout << "PUVODNI___________ " << source << "_________\n";
  std::cout << printExprTree(ast) << "\n";
  std::cout << "\n" << prefixPrint(ast) << "\n";
  std::cout << "__________________________________________\n";
  std::cout << "\nProgram output:\n\n";

  //seznam instrukcí, co lze vykonat
  std::vector<Instruction> program = {};
  emit(program, ast);
  //stack, na kterém to actually poběží
  std::vector<int> stack = {};
  //proměnné, které to bude mít k dispozici, žijí zde
  std::unordered_map<std::string, int> vars;
  interpret(program, stack, vars);
  cout << "Stack length: " << stack.size() << "\n";
  for(auto el: stack){
    cout << el << "\n";
  }
  cout << "Hello world\n"; //abych si mohl dát breakpoint za interpret
}