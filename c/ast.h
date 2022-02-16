#include <stdio.h>
//#define print(t,v) printf("%-10s 0x%lx\n",t,v)
#define print(t,v) printf("%-10s 0x%lx %d %s\n",t,v,short_class(v),printString(v))
typedef unsigned long objectT;
#define negative_inf 0xfff0000000000000
#define false 0xfff4000000010000
#define true 0xfff6000000100001
#define nil 0xfff8100000000000
#define symbol_of(index,arity) ((index)|((arity)<<20)|(0x7ffdl<<49))
#define from_object(addr) ((((long)(void*)addr)>>3)|(0x7ff8l<<49))
#define from_closure(addr) ((((long)(void*)addr)>>3)|(0x7ff9l<<49))
#define character_of(c) ((objectT)(c))|(0x7ffel<<49)
#define from_int(x) (((objectT)(x))+int_zero)
#define int_zero 0xffff000000000000
static inline objectT from_double(const double x) {
  union {objectT ul;double d;} y;
  y.d=x;
  return y.ul;
}
#define as_int(x) (((long)(x))-int_zero)
static inline double as_double(const objectT x) {
  union {objectT ul;double d;} y;
  y.ul=x;
  return y.d;
}
#define as_pointer(x) (((long)(x))<<15>>12)
#define get_class(x) (((objectT)(x)>>49)>0x7ff8?((int)((objectT)(x)>>49)&7):((objectT)(x)>>49)<0x7ff0?8:get_class_7ff0(x))
#define short_class(x) (((objectT)(x))>0xfff0000000000000?((int)((objectT)(x)>>49)&7):8)
static inline int get_class_7ff0(const objectT x) {
  long ptr = as_pointer(x);
  if (ptr==0) return 8;
  ptr = *((long *)ptr);
  if (ptr>0) return ptr&0xffff;
  __builtin_trap();
}
static char * printString(objectT x) {
  static char result[100];
  switch (short_class(x)) {
  case 0:
    snprintf(result,sizeof(result),"object 0x%lx",as_pointer(x));
    break;
  case 1:
    snprintf(result,sizeof(result),"closure 0x%lx",as_pointer(x));
    break;
  case 2:
    snprintf(result,sizeof(result),"false");
    break;
  case 3:
    snprintf(result,sizeof(result),"true");
    break;
  case 4:
    snprintf(result,sizeof(result),"nil");
    break;
  case 5:
    snprintf(result,sizeof(result),"#%s",symbol_table[x&0xfffff]);
    break;
  case 6:
    snprintf(result,sizeof(result),"$%c",(char)(x&0xffffff));
    break;
  case 7:
    snprintf(result,sizeof(result),"%ld",as_int(x));
    break;
  case 8:
    snprintf(result,sizeof(result),"%g",as_double(x));
    break;
  }
  return result;
}
