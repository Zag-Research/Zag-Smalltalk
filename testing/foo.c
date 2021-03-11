#include <stdio.h>
int main(int argc,char ** argv) {
  long x = -(1L<<52);
  printf("%lu %g\n",sizeof x,*((double*)&x));
}
