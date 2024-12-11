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
	<< trueLabel asString << ','
	<< falseLabel asString
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
