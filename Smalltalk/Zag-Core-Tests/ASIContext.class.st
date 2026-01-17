"
I am a context for the Semantic Interpreter
"
Class {
	#name : 'ASIContext',
	#superclass : 'ASIStackHeapObject',
	#instVars : [
		'returnPC',
		'sendingContext',
		'contextData',
		'method'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'as yet unclassified' }
ASIContext >> callersReturnPC [
	sendingContext ifNotNil: [ ^ sendingContext returnPC ] ifNil:  [ self halt. "is this the topmost context and should I return an nullProgramCounter?" ^ASINullProgramCounter new ]
]

{ #category : 'accessing' }
ASIContext >> contextData [
	^ contextData
]

{ #category : 'accessing' }
ASIContext >> contextData: aContextData [

	contextData := aContextData
]

{ #category : 'accessing' }
ASIContext >> createContextData: anInterpreter [
   method ifNil: [ ^nil ].
	^anInterpreter createContextData: method
]

{ #category : 'accessing' }
ASIContext >> method: aBasicBlock [ 
	method := aBasicBlock 
]

{ #category : 'as yet unclassified' }
ASIContext >> populateFrom: interpreter [ 
	[sendingContext != interpreter top] do: [ 
		
	]
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
ASIContext >> sendingContext [

	^ sendingContext
]

{ #category : 'accessing' }
ASIContext >> sendingContext: aSemanticContext [

	sendingContext := aSemanticContext
]

{ #category : 'as yet unclassified' }
ASIContext >> withMethodOrBlock: aBasicBlock [ 
	
	(method == nil) ifTrue: [ ^ false ].
	^ method withMethodOrBlock: aBasicBlock
	
]
