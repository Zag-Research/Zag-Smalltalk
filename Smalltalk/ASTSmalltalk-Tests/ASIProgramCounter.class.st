"
I represent the program counter pointing into a compiled method.
"
Class {
	#name : 'ASIProgramCounter',
	#superclass : 'Object',
	#instVars : [
		'compiledMethod',
		'currentIndex'
	],
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'accessing' }
ASIProgramCounter >> compiledMethod: anObject [

	compiledMethod := anObject.
	currentIndex := 1
]

{ #category : 'as yet unclassified' }
ASIProgramCounter >> doCode: anInterpreter [

	| operation |
	operation := compiledMethod at: currentIndex.
	currentIndex := currentIndex + 1.
	^ anInterpreter perform: operation
]

{ #category : 'as yet unclassified' }
ASIProgramCounter >> doForInterpreter: anInterpreter [

	| operation |
	operation := compiledMethod at: currentIndex.
	currentIndex := currentIndex + 1.
	^ anInterpreter perform: operation
]

{ #category : 'as yet unclassified' }
ASIProgramCounter >> next [
	| object |
	object := compiledMethod at: currentIndex.
	currentIndex := currentIndex + 1.
	^ object
]
