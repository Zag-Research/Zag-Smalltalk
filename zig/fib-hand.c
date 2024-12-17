// fib
//   self <= 2 ifTrue: [^ 1]
//   ^ (self - 1) fib + (self - 2) fib
typedef unsigned long long u64;
typedef long long i64;
typedef short u16;
typedef union { u64 u; char a[8];} Object;
#define from(i) (cast(i + 0xFFFC000000000000LL))
#define cast(v) ((Object) {.u = v})
#define untaggedInt(o) (o.u - 0xFFFC000000000000LL)
typedef u64 HeapHeader;
#define header(x) (x)
#define contextHeaderOnStack(x) ((u64)119<<48|(x)<<16)
typedef struct CompiledMethod CompiledMethod;
typedef struct { Object top, next, third, fourth; } *SP;
typedef struct {unsigned int hash;unsigned short class;} MethodSignature;
#define ms(h, c) ((MethodSignature){.hash = h,.class = c})
MethodSignature noMS;
typedef struct Process Process;
typedef struct Context Context;
typedef union Code *PC;
typedef SP ThreadedFn(PC pc, SP sp, Process *process, Context *context, MethodSignature signature);
typedef union Code {ThreadedFn *prim; i64 i; u64 u; Object object;} Code;
#define stack_size 909
struct Process {
  Object stack[stack_size];
  Process *next;
  u64 id;
  u64 trapContextNumber;
  // and more that we don't care about
};
struct Context {
  HeapHeader header;
  CompiledMethod *method; // note this is not an Object, so access and GC need to handle specially
  PC tpc; // threaded PC
  ThreadedFn *npc; // native PC - in Continuation Passing Style
  Context *prevCtxt; // note this is not an Object, so access and GC need to handle specially
  u64 trapContextNumber;
  Object temps[1];
};
#define Context_Basesize 6
struct CompiledMethod {
  HeapHeader header;
  Object stackStructure;
  MethodSignature signature;
  ThreadedFn *verifier;
  Code code[3];
};
extern SP verifyMethod(PC, SP, Process*, Context *, MethodSignature);
SP fib(PC, SP, Process *, Context *, MethodSignature);
SP fib1(PC, SP, Process *, Context *, MethodSignature);
SP fib2(PC, SP, Process *, Context *, MethodSignature);
CompiledMethod fibCM = {header(0),from(0),ms(0,0),verifyMethod,{&fib,&fib1,&fib2}};
extern Context *push(Context*, SP, Process*, CompiledMethod*, u16, u16, u16);
static int isOnStack(Context *self) {
  return (self->header>>48&15)==0;
}
SP fib(PC pc, SP sp, Process *process, Context *context, MethodSignature signature) {
  Object self = sp->top;
  if (self.u <= from(2).u) {
    sp->top = from(1);
    return context->npc(context->tpc, sp, process, context, noMS);
  }
  Context *newContext = (Context*)(sp - Context_Basesize);
  if ((SP)newContext-2>=(SP)process) {
    newContext->header = contextHeaderOnStack(Context_Basesize + 0);
    newContext->method = &fibCM;
    newContext->prevCtxt = context;
    newContext->trapContextNumber = ((Process*)((u64)process&~0x1F))->trapContextNumber;
    context = newContext;
  } else {
    context = push(context, sp, process, &fibCM, 0, 2, 1);
  }
  sp = ((SP)context)-1;
  sp->top = cast(self.u-untaggedInt(from(1)));
  context->npc = &fib1;
  context->tpc = pc+1;
  return fib(pc-1,sp,process,context,signature);
}
SP fib1(PC pc, SP sp, Process *process, Context *context, MethodSignature signature) {
  Object self = context->temps[0];
  SP newSp = sp-1;
  newSp->top = cast(self.u-untaggedInt(from(2)));
  context->npc = &fib2;
  context->tpc = pc+1;
  return fib(pc-2,newSp,process,context,signature);
}
typedef struct {SP sp;Context *ctxt;} SPC;
SPC pop(Context*,Process*);
SP fib2(PC pc, SP sp, Process *process, Context *context, MethodSignature signature) {
  Object result = cast(sp->top.u+untaggedInt(sp->next));
  if (isOnStack(context)) {
      sp = ((SP)context)+Context_Basesize;
      context = context->prevCtxt;
    } else {
      SPC spAndContext = pop(context,process);
      sp = spAndContext.sp;
      context = spAndContext.ctxt;
    }
  sp->top = result;
  return context->npc( context->tpc, sp, process, context, signature);
}
