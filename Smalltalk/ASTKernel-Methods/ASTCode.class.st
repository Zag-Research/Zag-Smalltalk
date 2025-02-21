"
I represent a Method or a Block.

I may be interpreted directly by the interpreter, or I may be compiled.
"
Class {
	#name : 'ASTCode',
	#superclass : 'ASTObject',
	#instVars : [
		'name',
		'code',
		'enclosingScope',
		'argumentNames',
		'localNames'
	],
	#category : 'ASTKernel-Methods',
	#package : 'ASTKernel-Methods'
}

{ #category : 'as yet unclassified' }
ASTCode >> frameSize [
	"Answer the size of temporary frame needed to run the receiver."

	"NOTE:  Versions 2.7 and later use two sizes of contexts."

	(self header bitAnd: 16r20000) = 0
		ifTrue: [ ^ 16 ]
		ifFalse: [ ^ 56 ]
]

{ #category : 'as yet unclassified' }
ASTCode >> header [
	"Answer the method header word containing information about the form of this method (e.g., number of literals) and the context needed to run it."

	^ self objectAt: 1
]

{ #category : 'as yet unclassified' }
ASTCode >> initialPC [
	"Answer the program counter for my first bytecode."

	^ 4 * (self numLiterals + 1) + 1
]

{ #category : 'as yet unclassified' }
ASTCode >> isCompiledMethod [
	^ true
]

{ #category : 'as yet unclassified' }
ASTCode >> numLiterals [
	"Answer the number of literals used by the receiver."

	^ (self header bitShift: -9) bitAnd: 16rFF
]

{ #category : 'as yet unclassified' }
ASTCode >> numTemps [
	"Answer the number of temporary variables used by this method."

	^ (self header bitShift: -18) bitAnd: 16r3F
]

{ #category : 'as yet unclassified' }
ASTCode >> objectAt: index [
	"Primitive. Answer the method header (if index = 1) or a literal (if index > 1) from the receiver. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed
]

{ #category : 'as yet unclassified' }
ASTCode >> objectAt: index put: value [
	"Primitive. Store the value argument into a literal in the receiver. An index of 2 corresponds to the first literal. Fails if the index is less than 2 or greater than the number of literals. Answer the value as the result. Normally only the compiler sends this message because only the compiler stores values in CompiledMethods. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed
]
