"
I am the superclass of Zag objects for the Semantic Interpreter
"
Class {
	#name : 'ASIObject',
	#superclass : 'Object',
	#classVars : [
		'ZagArray',
		'ZagClosure',
		'ZagContext'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'class initialization' }
ASIObject class >> initialize [

	ZagClosure := 35.
	ZagContext := 36.
	ZagArray := 37
]

{ #category : 'class membership' }
ASIObject >> zagClass [

	^ self subclassResponsibility
]
