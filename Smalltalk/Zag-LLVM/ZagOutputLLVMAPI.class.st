"
I am a mock connection that somewhat documents how `ZagOutputLLVM` generates the JIT code.
"
Class {
	#name : 'ZagOutputLLVMAPI',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'nextRegister',
		'nextLabel'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'adding' }
ZagOutputLLVMAPI >> add: aNumber to: selfValue [
	
	| result |
	result := self newRegister.
	stream 
	<< 'LLVMBuildAdd(*, '
	<< aNumber asString << ','
	<< selfValue asString << ','
	<< result 
	<< ')';
	cr.
	^ result.
	
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> and: valueRef1 with: valueRef2 [
	"may need to create constant"
   "on the zig side, if we have an Object x, then x.isInt() will return a (zig) boolean"
   "Update: if either operands = variable, then result = variable, else result = constant"

	| result |
	result := self newRegister.
	stream 
	<< 'LLVMBuildAnd(*,'
	<< valueRef1 asString << ','
	<< valueRef2 asString << ','
	<< result asString "result label"
	<< ')';
	cr.
	^ result
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> compare: valueRef1 is: comparison to: valueRef2 [

	| result |
	result := self newRegister. "result will be an i1"
	stream 
	<< 'LLVMBuildICmp(*,'
	<< comparison asString << ','
	<< valueRef1 asString << ','
	<< valueRef2 asString << ','
	<< result asString 
	<< ')';
	cr.
	^ result.
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [
	
	stream 
	<< 'LLVMBuildCondBr(*, '
	<< condition asString << ',' 
	<< 'label'<< trueLabel asString << ','
	<< 'label' << falseLabel asString
	<< ')';
	cr.
	
	
]

{ #category : 'initialization' }
ZagOutputLLVMAPI >> initialize [

	super initialize.
	stream := Transcript.
	nextRegister := 0.
	nextLabel := 0
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> newLabel [

	nextLabel := nextLabel + 1.
	stream 
	<< 'LLVMAppendBasicBlock( OriginatingFunc,'
	<< 'label' << nextLabel asString 
	<< ')';
	cr. 
	^ nextLabel
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> newRegister [

	nextRegister := nextRegister + 1.
	^ '%',nextRegister asString
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> or: int1 with: int2 [
	
	| result | 
	result := self newRegister.
	stream 
	<< 'LLVMBuildOr(*,'
	<< int1 asString << ','
	<< int2 asString << ','
	<< result 
	<< ')';
	cr.
	^ result. 
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> register: register plus: offset [
	
	| newRegister |
	newRegister := self newRegister.
	stream 
	<< 'LLVMBuildGEP2(*'
	<< '. . . )';
	cr.
	^ newRegister.
	
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> returnWith: addressOfValue [
	
	stream
	<< 'LLVMBuildRet(*, '
	<< addressOfValue 
	<< ')';
	cr. 
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> shiftLeftArithmeticly: num1 by: num2 [ 
	
	| result |
	result := self newRegister.
	stream
	<< 'LLVMBuildShl(*, '
	<< num1 asString << ','
	<< num2 asString << ','
	<< result 
	<< ')';
	cr.
	^ result.
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> shiftRightArithmeticly: num1 by: num2 [ 
	
	| result |
	result := self newRegister.
	stream
	<< 'LLVMBuildAShr(*, '
	<< num1 asString << ','
	<< num2 asString << ','
	<< result 
	<< ')';
	cr.
	^ result.
	
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> store: result at: addressOfSelf [
	
	stream 
	<< 'LLVMBuildStore(*, '
	<< result asString << ','
	<< addressOfSelf asString 
	<< ')';
	cr.
	
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> tagSmallInteger: untaggedInt [

	| result res1 res2 |
	result := self newRegister.
	res1 := self shiftLeftArithmeticly: untaggedInt by: 8.
	res2 := self or: untaggedInt with: 16rff.
	^ res1.
	 
	
	
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> valueAt: aString [

	| result |
	result :=  self newRegister.
	stream
	<< 'LLVMBuildLoad2(*,'
	<< 'LLVM.Type' << ','
	<< '%' << aString << ','
	<< result "label of result as a string"
	<< ')';
	cr.
	^ result
]
