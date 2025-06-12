#include <stdio.h>
//#define print(t,v) printf("%-10s 0x%lx\n",t,v)
#define print(v) ({typeof (v) V = (v);printf("%-25s 0x%lx %d %s\n",#v,V,short_class(V),printString(V));})
typedef unsigned long objectT;
#define NEGATIVE_INF 0xfff0000000000000
#define false 0xfff4000000010000
#define true 0xfff6000000100001
#define nil 0xfff8100000000000
#define symbol_of(index,arity) ((index)|((arity)<<20)|(0x7ffdl<<49))
#define from_object(addr) ((((long)(void*)addr))+(0x7ff8l<<49))
#define from_closure(addr) ((((long)(void*)addr))+(0x7ff9l<<49))
#define from_char(c) ((objectT)(c))|(0x7ffel<<49)
#define from_int(x) (((objectT)(x))+INT_ZERO)
#define INT_MINVAL 0xfffe000000000000
#define INT_ZERO   0xffff000000000000
#define INT_MAXVAL 0xffffffffffffffff
#define from_double(x) ({         \
  union {objectT ul;double d;} y_; \
  y_.d=x;                          \
  y_.ul;                           \
    })
#define as_int(x) (((long)(x))-INT_ZERO)
#define as_double(x) ({            \
  union {objectT ul;double d;} y_; \
  y_.ul=x;                         \
  y_.d;                            \
})
#define as_pointer(x) (((long)(x))<<12>>12)
#define get_class(x) (((objectT)(x)>>49)>0x7ff8?((int)((objectT)(x)>>49)&7):((objectT)(x)>>49)<0x7ff0?8:get_class_7ff0(x))
#define short_class(x) (((objectT)(x))>NEGATIVE_INF?((int)((objectT)(x)>>49)&7):8)
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
typedef objectT (*f0T)(objectT self);
typedef objectT (*f1T)(objectT self,objectT other);
typedef objectT (*f2T)(objectT self,objectT other,objectT other2);
typedef objectT (*f3T)(objectT self,objectT other,objectT other2,objectT other3);
typedef struct {objectT key;f0T func;} matchT;
typedef struct{long header;objectT name;objectT superclass;short size;objectT matches;} classT;
#define REF(name) {S_##name,(f0T)&M_##name}
#define REF2(sym,func) {S_##sym,(f0T)&func}
#define NILREF {nil,(f0T)0}
#define HEADER(format,numSlots,hash,classIndex) ((((long)(numSlots))<<48)|((long)(format)<<40)|((hash)<<16)|(classIndex))
#define DISPATCH_HEADER(name,size) {HEADER(15,size,S_##name&0xfffff,11+(S_##name&0xffff)),(f0T)from_object(&C_##name)}
#define CLASS_HEADER(name) HEADER(10,4,S_##name&0xfffff,11+(S_Class&0xffff))
#define DISPATCH(name) from_object(dispatch_##name)
#define CLASS(name,super,size) classT C_##name={CLASS_HEADER(name),S_##name,from_object(&C_##super),size,DISPATCH(name)}
#define METACLASS(name,super,size) classT C_M_##name={CLASS_HEADER(name),S_##name,from_object(&C_M_##super),size,DISPATCH(M_##name)}
