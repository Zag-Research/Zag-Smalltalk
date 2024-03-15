Class {
	#name : 'ASMessageT1',
	#superclass : 'Object',
	#instVars : [
		'field'
	],
	#category : 'ASTSmalltalk-Tests-Code',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Code'
}

{ #category : 'initialization' }
ASMessageT1 >> field [
	^ field
]

{ #category : 'initialization' }
ASMessageT1 >> initialize [
	super initialize.
	field := SharedRandom globalGenerator next.
 
]

{ #category : 'initialization' }
ASMessageT1 >> p1 [
	^ 40001
]
