#include <stdio.h>
//#define print(t,v) printf("%-10s 0x%lx\n",t,v)
#define print(t,v) printf("%-10s 0x%lx %d\n",t,v,get_class(v))
typedef unsigned long objectT;
#define negative_inf 0xfff0000000000000
#define false 0xfff4000000000000
#define true 0xfff6000000000001
#define nil 0xfff8000000000002
#define symbol_of(index,arity) ((index)|((arity)<<20)|(0x7ffdl<<49))
#define character_of(c) ((objectT)(c))|(0x7ffel<<49)
#define from_int(x) (((objectT)(x))|(0x7fffl<<49))
#define int_zero 0xfffe000000000000
static inline objectT from_double(const double x) {
  union {objectT ul;double d;} y;
  y.d=x;
  return y.ul;
}
#define as_int(x) (((long)(x))<<15>>15)
static inline double as_double(const objectT x) {
  union {objectT ul;double d;} y;
  y.ul=x;
  return y.d;
}
#define get_class(x) (((objectT)(x)>>49)>0x7ff0?((int)((objectT)(x)>>49)&7):((objectT)(x)>>49)<0x7ff0?8:get_class_7ff0(x))
static inline int get_class_7ff0(const objectT x) {
  long ptr = (((long)(x))<<15>>12);
  if (ptr==0) return 8;
  ptr = *((long *)ptr);
  if (ptr>0) return ptr&0xffff;
  __builtin_trap();
}
