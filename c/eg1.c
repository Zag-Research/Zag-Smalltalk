char *symbol_table[]={0,"sym1","yourself","bar:","sym4"};
#include "ast.h"
#include <math.h>
#define S_yourself symbol_of(2,0)
#define S_bar_ symbol_of(3,1)
#define S_Object symbol_of(4,0)
#define S_Magnitude symbol_of(5,0)
#define S_Number symbol_of(5,0)
#define S_SmallInteger symbol_of(6,0)
#define S_negated symbol_of(7,0)
objectT M_intplus_(objectT self,objectT other){
  return self;
}
objectT M_negated(objectT self){
  objectT result=from_int(0-as_int(self));
  // if (result==0) overflow;
  return result;
}
objectT M_yourself(objectT self){
  return self;
}
matchT dispatch_Object[]={REF(yourself),NILREF};
matchT dispatch_Number[]={NILREF};
matchT dispatch_Magnitude[]={NILREF};
matchT dispatch_SmallInteger[]={REF(yourself),REF(negated),REF2(bar_,M_intplus_),NILREF};
classT C_Object={CLASS_HEADER(Object),S_Object,nil,0,DISPATCH(Object)};
/*CLASS(BlockClosure,,0);
CLASS(False,,0);
CLASS(True,,0);
CLASS(UndefinedObject,,0);
CLASS(Symbol,,0);
CLASS(Character,,0);
*/
classT C_Number;
CLASS(SmallInteger,Number,0);
/*CLASS(Float,,0);
CLASS(Array,,0);
CLASS(String,,0);
CLASS(Class,,0);
CLASS(Metaclass,,0);
CLASS(Behavior,,0);
CLASS(Method,,0);
CLASS(System,,0);
*/
CLASS(Magnitude,Object,0);
CLASS(Number,Magnitude,0);
/*CLASS(Return,,0);
CLASS(Send,,0);
CLASS(Literal,,0);
CLASS(Load,,0);
CLASS(Store,,0);
*/
int main(int argc,char **argv) {
  print(from_double(-INFINITY)+1);
  print(from_object(&main));
  print(from_object(&argc));
  print(from_closure(&main));
  print(false);
  print(true);
  print(nil);
  print(S_yourself);
  print(S_bar_);
  print(from_char('c'));
  print(from_int(-42));
  print(from_int(42));
  print(from_double(42.7));
  print(from_double(INFINITY));
  print(from_double(-INFINITY));
  return 0;
}
