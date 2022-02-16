char *symbol_table[]={0,"sym1","foo","bar:","sym4"};
#include "ast.h"
#include <math.h>
#define S_foo symbol_of(2,0)
#define S_bar_ symbol_of(3,1)
int main(int argc,char **argv) {
  print(from_double(-INFINITY)+1);
  print(from_object(&main));
  print(from_object(&argc));
  print(from_closure(&main));
  print(false);
  print(true);
  print(nil);
  print(S_foo);
  print(S_bar_);
  print(from_char('c'));
  print(from_int(-42));
  print(from_int(42));
  print(from_double(42.7));
  print(from_double(INFINITY));
  print(from_double(-INFINITY));
  return 0;
}
