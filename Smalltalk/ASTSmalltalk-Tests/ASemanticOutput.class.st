"
I am an outputer strictly for `ASemanticInterpreter`
"
Class {
	#name : 'ASemanticOutput',
	#superclass : 'ASOutput',
	#instVars : [
		'code'
	],
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'accessing' }
ASemanticOutput >> at: anInteger [

	^ code at: anInteger
]

{ #category : 'accessing' }
ASemanticOutput >> codeSize [

	^ code size
]

{ #category : 'initialization' }
ASemanticOutput >> initialize [

	super initialize.
	code := OrderedCollection new
]

{ #category : 'code generation' }
ASemanticOutput >> outputBasicBlockEnd [

]

{ #category : 'code generation' }
ASemanticOutput >> outputMethodDefinition: aString primitive: anUndefinedObject methods: aCollection [ 
]

{ #category : 'code generation' }
ASemanticOutput >> outputMethodStart: aBasicBlock [
]

{ #category : 'as yet unclassified' }
ASemanticOutput >> startingPc [

	code := code asArray.
	^ ASIProgramCounter new compiledMethod: self
]

{ #category : 'API' }
ASemanticOutput >> visitPushBlock: anASCBlock [

	^ self halt
]

{ #category : 'API' }
ASemanticOutput >> visitPushLiteral: aLiteral [

	code add: #pushLiteral; add: aLiteral
]

{ #category : 'API' }
ASemanticOutput >> visitReturnTop: anOperation [

	code add: #returnTOS
]
