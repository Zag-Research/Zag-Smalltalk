"
I represent a local variable for the method
"
Class {
	#name : 'ASCLocal',
	#superclass : 'ASCParameter',
	#instVars : [
		'modMin',
		'modMax'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'testing' }
ASCLocal >> isMutable [

	^ true
]

{ #category : 'affecting' }
ASCLocal >> pop: output [

	^ output visitPopLocal: self
]

{ #category : 'affecting' }
ASCLocal >> push: output [

	^ output visitPushLocal: self
]

{ #category : 'affecting' }
ASCLocal >> store: output [

	^ output visitStoreLocal: self
]

{ #category : 'state tracking' }
ASCLocal >> store: anObject at: depth [

	modMin := modMin ifNil: [ depth ] ifNotNil: [depth min: modMin].
	modMax := modMax ifNil: [ depth ] ifNotNil: [depth min: modMax].
	^ self store: anObject
]
