#include "ast.h"

int main(int argc,char **argv) {
  print("42",from_int(42));
  print("42.0",from_double(42.0));
  print("nil",nil);
  return 0;
}
