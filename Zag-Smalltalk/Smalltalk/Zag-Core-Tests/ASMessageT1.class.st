Class {
	#name : 'ASMessageT1',
	#superclass : 'Object',
	#instVars : [
		'field'
	],
	#category : 'Zag-Core-Tests-Code',
	#package : 'Zag-Core-Tests',
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
