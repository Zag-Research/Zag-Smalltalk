#include "ast.h"
#include <math.h>
#define S_foo symbol_of(42,0)
#define S_bar_ symbol_of(43,1)
int main(int argc,char **argv) {
  print("object",from_double(-INFINITY)+1);
  print("false",false);
  print("true",true);
  print("nil",nil);
  print("S_foo",S_foo);
  print("S_bar_",S_bar_);
  print("$c",character_of('c'));
  print("42",from_int(42));
  print("42.0",from_double(42.0));
  print("inf",from_double(INFINITY));
  print("-inf",from_double(-INFINITY));
  return 0;
}
