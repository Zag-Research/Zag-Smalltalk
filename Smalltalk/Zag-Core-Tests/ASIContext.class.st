"
I am a context for the Semantic Interpreter
"
Class {
	#name : 'ASIContext',
	#superclass : 'ASIStackHeapObject',
	#instVars : [
		'returnPC',
		'sendingContext',
		'extra',
		'selfObject'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'as yet unclassified' }
ASIContext >> baseAddress [

	isOnStack ifFalse: [ ^ nil ].
	^ offset - extra
]

{ #category : 'accessing' }
ASIContext >> extra: anInteger [

	extra := anInteger
]

{ #category : 'initialization' }
ASIContext >> initialize [

	super initialize.
	extra := 0
]

{ #category : 'accessing' }
ASIContext >> returnPC [

	^ returnPC
]

{ #category : 'accessing' }
ASIContext >> returnPC: aProgramCounter [

	returnPC := aProgramCounter
]

{ #category : 'accessing' }
ASIContext >> selfObject [

	^ selfObject
]

{ #category : 'accessing' }
ASIContext >> selfObject: anObject [

	selfObject := anObject
]

{ #category : 'accessing' }
ASIContext >> sendingContext [

	^ sendingContext
]

{ #category : 'accessing' }
ASIContext >> sendingContext: aSemanticContext [

	sendingContext := aSemanticContext
]
