#include <stdio.h>
int main(int argc,char ** argv) {
  char *header="ASTimage";
  long x = -(1L<<57);
  printf("%lu %g\n",sizeof x,*((double*)&x));
  printf("%lx %s\n",*((long *)header),header);
}
