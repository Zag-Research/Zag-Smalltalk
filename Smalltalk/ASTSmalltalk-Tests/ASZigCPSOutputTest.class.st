Class {
	#name : 'ASZigCPSOutputTest',
	#superclass : 'ASZigTest',
	#instVars : [
		'output'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigCPSOutputTest >> setUp [

	super setUp.
	output := ASZigCPSOutput testing context: file
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_addTwoNumbers [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #addTwoNumbers.
	output generateCode: ast.

	self assert: output name equals: 'MaddTwoNumbers'.
	output asString inspect
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_fib [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #fib:.
	output generateCode: ast.
	self assert: output name equals: 'Mfib:'.
	file asString inspect.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_fib2 [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #fib2:.
	output generateCode: ast.
	self assert: output name equals: 'Mfib2:'.
	file asString inspect.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_simple1 [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #simple1.
	output generateCode: ast.
	self assert: output name equals: 'Msimple1'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Msimple1:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_simpleBlock [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #simpleBlock.
	output generateCode: ast.
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

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_unaries [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #unaries.
	output generateCode: ast.
	self assert: output name equals: 'Munaries'.
	file asString inspect
]

{ #category : 'tests' }
ASZigCPSOutputTest >> testZig_unariesPlus [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #unariesPlus.
	output generateCode: ast.
	self assert: output name equals: 'MunariesPlus'.
	file asString inspect
]
