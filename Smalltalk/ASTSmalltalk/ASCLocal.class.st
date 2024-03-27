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
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #testing }
ASCLocal >> isMutable [

	^ true
]

{ #category : #affecting }
ASCLocal >> pop: output [

	^ output visitPopLocal: self
]

{ #category : #affecting }
ASCLocal >> store: output [

	^ output visitStoreLocal: self
]
