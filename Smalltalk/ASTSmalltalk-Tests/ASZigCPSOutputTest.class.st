Class {
	#name : #ASZigCPSOutputTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCPSOutputTest >> testZig_addTwoNumbers [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #addTwoNumbers.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.

	self assert: output name equals: 'MaddTwoNumbers'.
	output asString inspect.

]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_fib [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		             context: file;
		             generateCode: ast.
	self assert: output name equals: 'Mfib:'.
	file asString inspect.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_fib2 [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib2:.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'Mfib2:'.
	file asString inspect.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_simple1 [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'Msimple1'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Msimple1:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_simpleBlock [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #simpleBlock.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'MsimpleBlock'.
	file asString inspect.
	self
		assert: output asString
		equals:
			'fn @"MsimpleBlock:1"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
if(!sym.simpleBlock.hashEquals(selector))return @call(tailCall,dnu,.{pc,sp,process,context,selector,cache});
ABLOCKvar sp_=sp.push(Object.from(42));
context.setReturn(&MsimpleBlock.code[1]);
const pc_=dispatch.lookup(Sym.@"value:", sp_.at(1).classIndex);
return @call(tailCall,pc_.prim,.{pc_.next(),sp_, process, context, Sym.@"value:", cache });
}
fn @"MsimpleBlock:2"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
const top_=sp.top;
const result_=context.pop(process);
var sp_=result_.sp;
sp_.top=top_;
const cContext_=result_.ctxt;
return @call(tailCall, cContext.npc, .{ cContext.tpc, sp_, process, callerContext, selector, cache });
}
var MsimpleBlock = compileMethod(Sym.i_1,nil,2,.{&@"MsimpleBlock:1",&@"MsimpleBlock:2", });'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unaries [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #unaries.
	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'Munaries'.
	file asString inspect

]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unariesPlus [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #unariesPlus.
	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'MunariesPlus'.
	file asString inspect
]
