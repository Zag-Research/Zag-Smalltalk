"
I am a context for the Semantic Interpreter
"
Class {
	#name : 'ASIContext',
	#superclass : 'ASIStackHeapObject',
	#instVars : [
		'returnPC',
		'sendingContext'
	],
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'accessing' }
ASIContext >> returnPC [

	^ returnPC
]

{ #category : 'accessing' }
ASIContext >> returnPC: aProgramCounter [

	returnPC := aProgramCounter
]

{ #category : 'accessing' }
ASIContext >> sendingContext: aSemanticContext [

	sendingContext := aSemanticContext
]
