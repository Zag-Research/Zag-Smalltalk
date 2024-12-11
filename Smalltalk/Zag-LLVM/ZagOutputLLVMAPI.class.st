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
	" may need to create constant"
" on the zig side, if we have an Object x, then x.isInt() will return a (zig) boolean"

	| result |
	result := self newRegister.
	stream 	<< result
	 << 'LLVMBuildAnd(*,'
	<< valueRef1 asString
	<< ','
	<< valueRef2 asString
	<< ')';
	cr.
	^ result
]

{ #category : 'llvm interface' }
ZagOutputLLVMAPI >> compare: valueRef1 is: comparison to: valueRef2 [

	| result |
	result := self newRegister.
	" result will be an i1 "
	^ result
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
	stream 	<<  result
	<< 'LLVMLoadInst(*,'
	<< '%'
	<< aString
	<< ')';
	cr.
	^ result
]
