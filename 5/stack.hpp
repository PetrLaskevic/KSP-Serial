

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
};

struct Instruction {
  //typ instrukce
  Opcode op;

  //nektere instrukce si potrebuji
  //pamatovat dalsi hodnoty
  //variant umoznuje mit v policku value ruzne typy: int, string, nebo monostate (= default placeholder)
  //cilem potom je k tomu mit type safe moznost jak k tomu pristupovat
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

//nově kvůli podpoře funkcí bude rekurzivní
int interpret(
    std::map<std::string, Function> const &program,
    std::string name,
    vector<int> &zasobnik,
    unordered_map<std::string, int> &promenne) {

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
      println("PRINT: {}", zasobnik.back());
    } break;

    case OP_LOAD: {
      //da hodnotu promenne .value (treba "x") na zasobnik
      //pokud proměnná neexistuje, tak ji v unordered_map vytvoří, s defaultní hodnotou typu
      //accessing a non-existent key by using [], will add new entry in the
      // map with your key and default value of your value type(or created with
      // non-argument constructor); src: https://comp.lang.cpp.moderated.narkive.com/ijAtjHG0/unordered-map-non-existent-key
      //takže kontroluju, jestli tam je (c++ 20)
      if(!promenne.contains(get<string>(ins.value))){
        cerr << "ERROR: Proměnná '" << get<string>(ins.value) << "' nebyla deklarována! (pro deklaraci `var a;` (výchozí hodnota je 0))\n";
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
      zasobnik.push_back(get<int>(ins.value));
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
      int second = zasobnik.back();
      //pop_back v C++ nevraci hodnotu returned elementu, proto je to rozdelene do zasobnik.back() a zasobnik.pop_back()
      zasobnik.pop_back();
      int first = zasobnik.back();
      zasobnik.pop_back();

      zasobnik.push_back(first + second);
    } break;
    case OP_SUB: {
      //sebere 2 hodnoty ze zasobniku, a da na zasobnik jejich rozdil
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_SUB!\n";
        break;
      }
      //  druhý operand byla první hodnota ze zásobníku a první operand druhá hodnota ze zásobníku
      int second = zasobnik.back();
      zasobnik.pop_back();
      int first = zasobnik.back();
      zasobnik.pop_back();
      zasobnik.push_back(first - second);

    } break;
    case OP_MUL: {
      // Implementuje integer multiplication
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_MUL!\n";
        break;
      }

      int second = zasobnik.back();
      zasobnik.pop_back();
      int first = zasobnik.back();
      zasobnik.pop_back();
      zasobnik.push_back(second * first);
    } break;
    case OP_DIV: {
      // Implementuje integer division (tedy zaukrouhleni dolu)
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_MUL!\n";
        break;
      }
      int second = zasobnik.back();
      zasobnik.pop_back();
      int first = zasobnik.back();
      zasobnik.pop_back();
      if(second == 0){
        cout << "Pozor, deleni 0\n";
        std::exit(1);
      }
      zasobnik.push_back(first / second);

    } break;
    case OP_DUP: {
      // Odebere hodnotu ze zasobniku a hned ji tam 2x prida
      // int val = zasobnik.back();
      // zasobnik.pop_back();
      // zasobnik.push_back(val);
      // zasobnik.push_back(val);
      //=nemusi ji vubec odebirat:
      int val = zasobnik.back();
      zasobnik.push_back(val);
    } break;

    case OP_BRANCH: {
     int podminka = zasobnik.back();
     zasobnik.pop_back();
     if(podminka){
      ip = get<int>(ins.value);
      //continue abychom se vyhnuli ip+=1 dole
      continue;
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
      int value = zasobnik.back();
      zasobnik.pop_back();
      //ne nutně 1, cokoliv truthy
      if(value >= 1){
        value = 0;
      }else if (value == 0){
        value = 1;
      }
      zasobnik.push_back(value);
     
    //tez funguje, chtelo se mi si trosku pohrat s C++
    //   if(zasobnik.size() > 0){
    //     //vector.back() can be used to assign a value to the last element of a vector
    //     // => as it returns a reference to the last element. Better make sure it's not empty first! //https://stackoverflow.com/questions/13851367/can-vector-back-be-used-to-assign-a-value-to-the-last-element-of-a-vector
    //     zasobnik.back() = !zasobnik.back();
    //   }
    } break;

    case OP_EQ: {
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_EQ!\n";
        break;
      }
      int a = zasobnik.back();
      zasobnik.pop_back();
      int b = zasobnik.back();
      zasobnik.pop_back();
      zasobnik.push_back(a == b);
    } break;  

    case OP_LT: {
      //pr pokud v zasobniku jenom jeden item, tak to muze popovat nahodny bordel (a fun veci jako vector size -1, najednou se tam objevily itemy lol)
      if(zasobnik.size() < 2){
        cout << "Pozor, na zasobniku nejsou alespon dve hodnoty, OP_LT!\n";
        break;
      }
      int first = zasobnik.back();
      zasobnik.pop_back();
      int second = zasobnik.back();
      zasobnik.pop_back();
      zasobnik.push_back(second < first);
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
      unordered_map<std::string, int> nove_promenne;
      //pozpátku protože args je        "a","b"
      //           a zasobnik je   ... , 6,  5
      for(std::string arg: args | std::ranges::views::reverse){
        nove_promenne[arg] = zasobnik.back();
        zasobnik.pop_back();
      }
      cout << "Alles klar?\n";
      std::vector<Instruction> code = call.code;
      //chceme aby nějak fungovaly věci typu var a = b(c);
      //=> to budou, stačí zavolat interpret(),
      //který na zásobníku svrchu nechá výslednou hodnotu
      interpret(program, callName, zasobnik, nove_promenne);
      cout << "Je pravda? " << (zasobnik.size() > 0) << "\n";
      //nechceme ale sem dávat return interpret() nebo tak,
      //protože bychom skončili, a další instrukce ve funkci po function callu by se už nevykonaly  
      // int callReturnValue = zasobnik.back();
      // zasobnik.pop_back();
      // return callReturnValue;

    }

    }
    ip += 1;
  }
}
