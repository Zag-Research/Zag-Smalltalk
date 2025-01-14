"
I am a mock connection that somewhat documents how `ZagOutputLLVM` generates the JIT code.
"
Class {
	#name : 'ZagOutputLLVMTextual',
	#superclass : 'ZagOutputLLVMAPI',
	#instVars : [
		'stream',
		'nextRegister',
		'nextLabel'
	],
	#category : 'Zag-LLVM-Tests-Support',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Support'
}

{ #category : 'adding' }
ZagOutputLLVMTextual >> add: aNumber to: selfValue [
	
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
ZagOutputLLVMTextual >> and: valueRef1 with: valueRef2 [
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
ZagOutputLLVMTextual >> compare: valueRef1 is: comparison to: valueRef2 [

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

{ #category : 'accessing' }
ZagOutputLLVMTextual >> contents [

	^ stream contents
]

{ #category : 'llvm interface' }
ZagOutputLLVMTextual >> defineLabel: aLabel [

	(stream << 'L' << aLabel asString)
		<< ':';
		cr
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMTextual >> if: condition isFalseThen: falseLabel isTrueThen: trueLabel [
	
	stream 
	<< 'LLVMBuildCondBr(*, '
	<< condition asString << ',' 
	<< 'label'<< trueLabel asString << ','
	<< 'label' << falseLabel asString
	<< ')';
	cr.
	
	
]

{ #category : 'initialization' }
ZagOutputLLVMTextual >> initialize [

	super initialize.
	stream := WriteStream on: ''.
	nextRegister := 0.
	nextLabel := 0
]

{ #category : 'instance creation' }
ZagOutputLLVMTextual >> newLabel [

	nextLabel := nextLabel + 1.
	stream 
	<< 'LLVMAppendBasicBlock( OriginatingFunc,'
	<< 'label' << nextLabel asString 
	<< ')';
	cr. 
	^ nextLabel
]

{ #category : 'llvm interface' }
ZagOutputLLVMTextual >> newRegister [

	nextRegister := nextRegister + 1.
	^ '%',nextRegister asString
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMTextual >> or: int1 with: int2 [
	
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
ZagOutputLLVMTextual >> register: register plus: offset [
	
	| newRegister |
	newRegister := self newRegister.
	stream 
	<< 'LLVMBuildGEP2(*'
	<< '. . . )';
	cr.
	^ newRegister.
	
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMTextual >> returnWith: addressOfValue [
	
	stream
	<< 'LLVMBuildRet(*, '
	<< addressOfValue 
	<< ')';
	cr. 
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMTextual >> shiftLeftArithmeticly: num1 by: num2 [ 
	
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
ZagOutputLLVMTextual >> shiftRightArithmeticly: num1 by: num2 [ 
	
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
ZagOutputLLVMTextual >> store: result at: addressOfSelf [
	
	stream 
	<< 'LLVMBuildStore(*, '
	<< result asString << ','
	<< addressOfSelf asString 
	<< ')';
	cr.
	
]

{ #category : 'as yet unclassified' }
ZagOutputLLVMTextual >> tagSmallInteger: untaggedInt [

	| result res1 res2 |
	result := self newRegister.
	res1 := self shiftLeftArithmeticly: untaggedInt by: 8.
	res2 := self or: untaggedInt with: 16rff.
	^ res1.
	 
	
	
]

{ #category : 'llvm interface' }
ZagOutputLLVMTextual >> valueAt: aString [

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
