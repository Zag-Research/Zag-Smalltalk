"
I represent a local variable for the method
"
Class {
	#name : #ASCLocal,
	#superclass : #ASCParameter,
	#instVars : [
		'modMin',
		'modMax'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #testing }
ASCLocal >> isMutable [

	^ true
]

{ #category : #'state tracking' }
ASCLocal >> store: anObject at: depth [

	modMin := modMin ifNil: [ depth ] ifNotNil: [depth min: modMin].
	modMax := modMax ifNil: [ depth ] ifNotNil: [depth min: modMax].
	^ self store: anObject
]
