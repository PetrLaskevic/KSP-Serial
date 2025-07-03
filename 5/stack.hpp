#include <cmath>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <map>
#include <ranges>

/*
Zadani zde: https://ksp.mff.cuni.cz/h/ulohy/37/zadani1.html#task-37-1-S
Vyrobil jsem zde zasobnikovy stroj,
do ktereho jsem implementoval dostatecne instrukce na to, 
aby v nich byl napsan program pocitajici faktorial.
*/

//misto #include <print> protoze mam stare g++ (13.2.0, kompilovano C++20 na linuxu)
//potrebuje alespon C++20, ve VS Code uz by to melo byt nastavene v .vscode slozce v tasks.json
#include <format>
#include <iostream>
using namespace std;
template<class... Args>
void print(format_string<Args...> fmt, Args&&... args){
  cout << format(fmt, forward<Args>(args)...);
}

template<class... Args>
void println(format_string<Args...> fmt, Args&&... args){
  print(fmt, forward<Args>(args)...);
  print("\n");
}


enum Opcode {
  OP_PRINT,
  OP_PUSH,
  OP_POP,

  OP_LOAD, //da hodnotu promenne .value (treba "x") na zasobnik
  OP_STORE,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_DUP,
  OP_BRANCH,
  OP_NOT,
  OP_EQ,
  OP_LT,
  OP_NOP,
  OP_CALL,
  OP_RET,
  OP_INDEX, //pro str[2]
};

struct Instruction {
  //typ instrukce
  Opcode op;

  //nektere instrukce si potrebuji
  //pamatovat dalsi hodnoty
  //variant umoznuje mit v policku value ruzne typy: int, string, nebo monostate (= default placeholder)
  //cilem potom je k tomu mit type safe moznost jak k tomu pristupovat
  
  //z nějakého důvodu se to úplně rozbije, když bych chtěl mít tady Variable
  variant<monostate, int, string> value = monostate{};

  //pak funkce std::get<string>(ins.value) z toho vezme string, pokud tam je, a jinak hodi error
  //So, std::get<string>(ins.value) retrieves the string value stored in the ins.value variant, assuming that ins.value indeed holds a string.
  //If ins.value does not contain a string, this will throw an exception of type std::bad_variant_access
  
};

vector<Instruction> instrukce;
vector<int> stack;
unordered_map<string, int> promenne;
int ip;

struct Function {
  //to znamená, že jsme si v callech povolili jenom promenne a ne vyrazy
  std::vector<std::string> args;
  std::vector<Instruction> code;
};

// na to je potřeba vědět typ
// typedef std::variant<int, std::string> Variable;

enum VarType {
  INSIDE_STRING_CHAR_COPY,
  NUMBER,
  STRING,
  NOTHING
};

struct Variable {

  std::variant<char, int, std::string> value;

  Variable(std::string v): value(v) { 
    // std::cout << "bežel string konstruktor\n";
  }
  Variable(int i): value(std::in_place_index<1>, i) {
    // std::cout << "bežel int konstruktor\n";
  } //value(i) nefunguje
  //tohle urcite nebude fungovat napric funkcemi - a ani pri volani funkci
  //var a = "ahoj";
  //foo(a[2]);
  //=> vlastne blbost, protoze když se emituje call, tak ještě ty proměnné máme
  //spíš (možná) zkázat return a[2]; => pokud to je pointer tak při použití v jiné funkci přestane existovat 
  //=> coz byl fail prechoziho commitu, ze ty veci prestavaly existovat, 7
  //a byl by to i fail s Variable(unordered_map<std::string, Variable> variables, std::string name, int i) variantou
  //proto takto jednoduse:
  Variable(char c): value(c) {
    // std::cout <<"bežel string index (char) konstruktor\n";
  }
  //s tím se pak pojí i to, že tam musí být nějaký default constructor (dostal jsem "no matching function for call to ‘Variable::Variable()")
  Variable() : value(0) {
    //běží když v OP_STORE: promenne[get<string>(ins.value)] = zasobnik.back();
    //se ta hodnota asi kopíruje => hodnoty však jsou správný a ne 0 => idk
    //(zkoumal jsem to tak, že jsem si dal na tenhle cout breakpoint a pak jsem se díval na call stack, 
    //a hledal tam poslední funkci, která je ještě moje a ne CPP interní
    //==> je to interpret, pro rozkliknutí právě ta řádka v OP_STORE )
    // std::cout << "bežel tenhle defaultní konstruktor\n";
  }

  VarType type(){
    if(value.index() == 0) return INSIDE_STRING_CHAR_COPY;
    if(value.index() == 1) return NUMBER;
    else if(value.index() == 2) return STRING;
    else return NOTHING;
  }

};

//nově kvůli podpoře funkcí bude rekurzivní
Variable interpret(
    std::map<std::string, Function> const &program,
    std::string name,
    vector<Variable> &zasobnik,
    unordered_map<std::string, Variable> &promenne) {

  Function functionToRun = program.at(name);
  vector<Instruction> instrukce = functionToRun.code;
  int ip = 0; // index aktuální instrukce
              // (instruction pointer)
  while (true) {
    if (!(ip >= 0 && ip < ssize(instrukce)))
      break;

    auto ins = instrukce.at(ip);

    switch (ins.op) {
    case OP_PRINT: {
      auto el = zasobnik.back();
      if(el.type() == NUMBER){
        std::cout << "PRINT: " <<  get<int>(el.value) << "\n";
      }else if(el.type() == STRING){
        std::cout << "PRINT: " << get<string>(el.value) << "\n";
      }else if(el.type() == INSIDE_STRING_CHAR_COPY){;
        std::cout << "PRINTR: " << get<char>(el.value) << "\n";
      }
    } break;

    case OP_LOAD: {
      //da hodnotu promenne .value (treba "x") na zasobnik
      //pokud proměnná neexistuje, tak ji v unordered_map vytvoří, s defaultní hodnotou typu
      //accessing a non-existent key by using [], will add new entry in the
      // map with your key and default value of your value type(or created with
      // non-argument constructor); src: https://comp.lang.cpp.moderated.narkive.com/ijAtjHG0/unordered-map-non-existent-key
      //takže kontroluju, jestli tam je (c++ 20)
      if(!promenne.contains(get<string>(ins.value))){
        std::cerr << "ERROR: Proměnná '" << get<string>(ins.value) << "' nebyla deklarována! (pro deklaraci `var a;` (výchozí hodnota je 0))\n";
        //kdybych tady nereturnoval tak operátor [] ji v unordered_map vytvoří s hodnotou 0
        std::exit(1);
      }
      zasobnik.push_back(
          promenne[get<string>(ins.value)]);

    } break;

    case OP_STORE: {
      //da hodnotu ze zasobniku (pop) do promenne .value (treba "x")
      //=popuje do promenne specifikovane v .value hodnotu z zasobniku (=pop zasobniku)
      if(zasobnik.size() == 0){
        std::cerr << "Na zásobníku není žádná hodnota, OP_STORE do proměnné nelze provést!\n";
        std::exit(1);
      }
      promenne[get<string>(ins.value)] = zasobnik.back();
      zasobnik.pop_back();
    } break;

    case OP_PUSH: {
      //da numerickou hodnotu (treba 8) na zasobnik
      //nebo nove i string literal - trochu jsem si pomohl tim, ze tam davam struct
      if(std::holds_alternative<int>(ins.value)){
        zasobnik.push_back(Variable(get<int>(ins.value)));
      }//nov povolím tady string => assumption, že to je string literal, pro proměnné máme OP_STORE
      else if(std::holds_alternative<std::string>(ins.value)){
        zasobnik.push_back(Variable(get<std::string>(ins.value)));
      }
    } break;

    case OP_POP: {
      if(zasobnik.size() < 1){
        cout << "Pozor, nelze pprovést OP_POP když je zásobník prázdný!\n";
      }
      // cout << "OP_POP\n";
      zasobnik.pop_back();
    } break;

    case OP_ADD: {
      //sebere 2 hodnoty ze zasobniku, a da na zasobnik jejich soucet
      //pro debugging:
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_ADD!\n";
        break;
      }
      Variable second = zasobnik.back();
      //pop_back v C++ nevraci hodnotu returned elementu, proto je to rozdelene do zasobnik.back() a zasobnik.pop_back()
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();

      if(first.type() == NUMBER && second.type() == NUMBER){
        zasobnik.push_back(Variable(get<int>(first.value) + get<int>(second.value)));
      }else if(first.type() == STRING && second.type() == STRING){
        zasobnik.push_back(Variable(get<std::string>(first.value) + get<std::string>(second.value)));
      }else if(first.type() == INSIDE_STRING_CHAR_COPY && second.type() == INSIDE_STRING_CHAR_COPY){
        std::string result = "  ";
        //get<char>(first.value) + get<char>(second.value) would just sum up the numbers
        //ASCII o + j = 111 + 106 == 217
        result[0] = get<char>(first.value);
        result[1] = get<char>(second.value);
        zasobnik.push_back(Variable(result));
      }else if(first.type() == INSIDE_STRING_CHAR_COPY && second.type() == STRING){
        zasobnik.push_back(get<char>(first.value) + get<std::string>(second.value));
      }else if(first.type() == STRING && second.type() == INSIDE_STRING_CHAR_COPY){
        zasobnik.push_back(get<std::string>(first.value) + get<char>(second.value));
      }else{
        std::cerr << "Nejsme JS, nebudeme míchat operandy string a int\n";
        std::exit(1);
      }
      
    } break;
    case OP_SUB: {
      //sebere 2 hodnoty ze zasobniku, a da na zasobnik jejich rozdil
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        std::cerr << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_SUB!\n";
        break;
      }
      //  druhý operand byla první hodnota ze zásobníku a první operand druhá hodnota ze zásobníku
      Variable second = zasobnik.back();
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();

      if(first.type() == NUMBER && second.type() == NUMBER){
        zasobnik.push_back(Variable(get<int>(first.value) - get<int>(second.value)));
      }else{
        std::cerr << "Odčítání podporujeme jenom na číslech";
        std::exit(1);
      }
    } break;
    case OP_MUL: {
      // Implementuje integer multiplication
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_MUL!\n";
        break;
      }

      Variable second = zasobnik.back();
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();

      if(first.type() == NUMBER && second.type() == NUMBER){
        zasobnik.push_back(Variable(get<int>(first.value) * get<int>(second.value)));
      }else if(first.type() == STRING && second.type() == NUMBER){
        std::string result;
        std::string repeatedString = get<std::string>(first.value);
        //číslo < 0 se implicitně chová jako 0, mohl bych dát warning
        for(int i = 0; i < get<int>(second.value); i++){
          result += repeatedString;
        }
        zasobnik.push_back(Variable(result));
      }else if(first.type() == NUMBER && second.type() == STRING){
        std::string result;
        std::string repeatedString = get<std::string>(second.value);
        for(int i = 0; i < get<int>(first.value); i++){
          result += repeatedString;
        }
        zasobnik.push_back(Variable(result));
      }else{
        std::cerr << "Stringy navzájem nenásobíme\n";
        std::exit(1);
      }

    } break;
    case OP_DIV: {
      // Implementuje integer division (tedy zaukrouhleni dolu)
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_MUL!\n";
        break;
      }

      Variable second = zasobnik.back();
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();
      if(first.type() == NUMBER && second.type() == NUMBER){
        if(get<int>(second.value) == 0){
          std::cerr << "Pozor, dělení 0\n";
          std::exit(1);
        }
        zasobnik.push_back(Variable(get<int>(first.value) / get<int>(second.value)));
      }else{
        std::cerr << "Stringy nedělíme!\n";
        std::exit(1);
      }
    } break;
    case OP_DUP: {
      // Odebere hodnotu ze zasobniku a hned ji tam 2x prida
      // int val = zasobnik.back();
      // zasobnik.pop_back();
      // zasobnik.push_back(val);
      // zasobnik.push_back(val);
      //=nemusi ji vubec odebirat:
      auto val = zasobnik.back();
      zasobnik.push_back(val);
    } break;

    case OP_BRANCH: {
     Variable podminka = zasobnik.back();
     zasobnik.pop_back();
     if(podminka.type() == NUMBER){
      if(get<int>(podminka.value)){
        ip = get<int>(ins.value);
        //continue abychom se vyhnuli ip+=1 dole
        continue;
      }
     }else{
      std::cerr << "OP_BRANCH vyžaduje číslo!\n";
      std::exit(1);
     }

    } break;

    case OP_NOP : {
        //Nedela nic
        //abych nemusel prepisovat indexy vsech nasludujicih branchu kdyz chci "comment out" bloku pri debugovani
        //tak ty instrukce nahradim NOP
    } break;

    case OP_NOT: {
      //sebere cislo ze zasobniku, a z 0 udela 1, z 1 udela 0
      if(zasobnik.size() == 0){
        cout << "Pozor, na zasobniku neni zadna hodnota, OP_NOT\n";
        break;
      }
      //tady už ať si to hodí výjimku, !"text" se nestane
      int value = get<int>(zasobnik.back().value);
      zasobnik.pop_back();
      //ne nutně 1, cokoliv truthy
      if(value >= 1){
        value = 0;
      }else if (value == 0){
        value = 1;
      }
      zasobnik.push_back(Variable(value));
    } break;

    case OP_EQ: {
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_EQ!\n";
        break;
      }
      Variable second = zasobnik.back();
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();

      if(first.type() == NUMBER && second.type() == NUMBER){
        zasobnik.push_back(Variable(get<int>(first.value) == get<int>(second.value)));
      }else if(first.type() == STRING && second.type() == STRING){
        zasobnik.push_back(Variable(get<string>(first.value) == get<string>(second.value)));
      }else{
        std::cerr << "Porovnání stringu a intu!\n";
        std::exit(1);
      }
      
    } break;  

    case OP_LT: {
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel (a fun veci jako vector size -1, najednou se tam objevily itemy lol)
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_LT!\n";
        break;
      }
      Variable second = zasobnik.back();
      zasobnik.pop_back();
      Variable first = zasobnik.back();
      zasobnik.pop_back();
      if(first.type() == NUMBER && second.type() == NUMBER){
        zasobnik.push_back(Variable(get<int>(first.value) < get<int>(second.value)));
      }else{
        std:cerr << "Nepodporujeme < mezi stringy nebo mezi stringem a číslem\n";
        std::exit(1);
      }
    } break;

    case OP_RET: {
      assert(!empty(zasobnik));
      return zasobnik.back();
    } break;

    case OP_CALL: {
      auto callName = std::get<string>(ins.value);
      Function call = program.at(callName);
      //nazvy paramatru, ktere ta funkce ma
      //jejich hodnoty pro ten call jsou na stacku
      std::vector<std::string> args = call.args;
      //asi chceme copy by value, ne by reference
      //taky chceme aby nekolidovaly nazvy promennych mezi funkcemi
      // => proto nove_promenne
      unordered_map<std::string, Variable> nove_promenne;
      //pozpátku protože args je        "a","b"
      //           a zasobnik je   ... , 6,  5
      for(std::string arg: args | std::ranges::views::reverse){
        nove_promenne[arg] = zasobnik.back();
        zasobnik.pop_back();
      }
      std::vector<Instruction> code = call.code;
      //chceme aby nějak fungovaly věci typu var a = b(c);
      //=> to budou, stačí zavolat interpret(),
      //který na zásobníku svrchu nechá výslednou hodnotu
      interpret(program, callName, zasobnik, nove_promenne);
      cout << "Velikost zasobniku: " << zasobnik.size() << "\n";
      //nechceme ale sem dávat return interpret() nebo tak,
      //protože bychom skončili, a další instrukce ve funkci po function callu by se už nevykonaly  
      // int callReturnValue = zasobnik.back();
      // zasobnik.pop_back();
      // return callReturnValue;
    } break;

    case OP_INDEX: {
      int index = get<int>(zasobnik.back().value);
      zasobnik.pop_back();
      //promenne[] tu promennou pokud neexistuje vytvoří, proto:
      //pro check bych mohl goto OP_LOAD (jestli switch podporuje GOTO)
      //nebo tu logiku zkopírovat:
      if(!promenne.contains(get<string>(ins.value))){
        std::cerr << "ERROR: Proměnná '" << get<string>(ins.value) << "' nebyla deklarována! (pro deklaraci `var a;` (výchozí hodnota je 0))\n";
        //kdybych tady nereturnoval tak operátor [] ji v unordered_map vytvoří s hodnotou 0
        std::exit(1);
      }
      Variable var = promenne[get<string>(ins.value)];
      if(std::holds_alternative<string>(var.value)){

        //moje oblibena Python feature
        if(abs(index) <= get<string>(var.value).length() && index < 0){
          index = get<string>(var.value).length() + index;
        }else if(index >= get<string>(var.value).length()){
          std::cerr << "Out of bounds, hodnota " << index << " pro literal '" << get<string>(var.value) << "'\n";  
          std::exit(1);
        }

        zasobnik.push_back(Variable(get<string>(var.value)[index]));
      }else{
        std::cout << var.type() << "\n";
        std::cerr << "Čísla zatím neindexujeme\n";
      }
    } break;

    }
    ip += 1;
  }
}
