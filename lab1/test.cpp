#include <iostream>
using namespace std;

/*
  programele iterative NU sunt REFERENTIAL TRANSPARENTE
  ++x nu poate fi inlocuit cu x + 1
  si -- x nu poate fi inlocuit cu x - 1
  comutativitatea operatiei de adunare nu este respectata

  int g = 0;
  int f() {
    return ++g;
  }
  f() - f() nu va returna 0!

  efectele laterale ingreuneaza intelegerea programului!
*/

int main() {
  int x = 0;
  cout << x-- + ++x << " " << x << " " << ++x + x-- << " " << x << endl;
  return 0;
}
