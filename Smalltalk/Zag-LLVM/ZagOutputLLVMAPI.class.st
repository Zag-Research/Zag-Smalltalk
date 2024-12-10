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

{ #category : 'as yet unclassified' }
ZagOutputLLVMAPI >> LLVMBuildAnd: valueRef1 and: valueRef2 [
	" may need to create constant"

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

{ #category : 'initialization' }
ZagOutputLLVMAPI >> initialize [

	super initialize.
	stream := Transcript.
	nextRegister := 0.
	nextLabel := 0
]

{ #category : 'testing' }
ZagOutputLLVMAPI >> is: aString equalTo: anInteger [

	^ self newRegister
]

{ #category : 'instance creation' }
ZagOutputLLVMAPI >> newLabel [

	nextLabel := nextLabel + 1.
	^ nextLabel
]

{ #category : 'instance creation' }
ZagOutputLLVMAPI >> newRegister [

	nextRegister := nextRegister + 1.
	^ '%',nextRegister asString
]

{ #category : 'accessing' }
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
