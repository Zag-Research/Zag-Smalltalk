"
I feed a (LLVM) genarator with a sequence of calls corresponding to a threaded sequence.

I emulate the zig code that is used to generate an LLVM-jitted native-code method from an existing threaded method.

That is, I send messages to the generator for each of the threaded codes in the threaded code for the method.
I also implement several helper functions required by such a generator.
"
Class {
	#name : 'ZagOutputLLVMFromTestArray',
	#superclass : 'ZagOutputSourceAPI',
	#instVars : [
		'index',
		'array',
		'address',
		'codeGen'
	],
	#classVars : [
		'HeaderSize'
	],
	#category : 'Zag-LLVM-Tests-Support',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Support'
}

{ #category : 'class initialization' }
ZagOutputLLVMFromTestArray class >> initialize [

	HeaderSize := 40
]

{ #category : 'private' }
ZagOutputLLVMFromTestArray >> addressOf: offset [

	^ address + HeaderSize + (offset - 1 * 8)
]

{ #category : 'private' }
ZagOutputLLVMFromTestArray >> addressOfLabel: anInteger [

	| target offset element |
	offset := anInteger.
	target := index.
	self halt.
	[
	(element := array at: target) isArray not or: [
		element first ~= #label or: [
			offset := offset - offset sign.
			offset ~= 0 ] ] ] whileTrue: [ target := target + offset sign ].
	^ self addressOf: target
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> codeGen: anObject [

	codeGen := anObject.
	anObject source: self
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> genFrom: collection [

	index := 1.
	array := collection.
	^ self iterate
]

{ #category : 'private' }
ZagOutputLLVMFromTestArray >> iterate [

	| selector |
	index > array size ifTrue: [ ^ nil ].
	selector := self valueOf.
	index := index + 1.
	codeGen perform: selector.
	^ self iterate
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> methodAddress: anObject [

	address := anObject
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> nextAddress [
" returns the next word as a Code address "

	| object |
	object := self valueOf.
	index := index + 1.
	^ object
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> nextObject [

	| object |
	object := self valueOf.
	index := index + 1.
	^ object
]

{ #category : 'API' }
ZagOutputLLVMFromTestArray >> thatAddress [

	^ self addressOf: index - 1
]

{ #category : 'private' }
ZagOutputLLVMFromTestArray >> valueOf [

	| object |
	object := array at: index.
	object isArray ifFalse: [ ^ object ].
	object first = #method ifTrue: [ ^ address ].
	object first = #label ifTrue: [ ^ #label ].
	object first isInteger ifTrue: [ ^ self addressOfLabel: object first ].
	^ self error: object
]
