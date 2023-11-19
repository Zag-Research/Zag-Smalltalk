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

{ #category : 'state tracking' }
ASCLocal >> store: anObject at: depth [

	modMin := depth min: modMin.
	modMax := depth max: modMax.
	^ self store: anObject
]
