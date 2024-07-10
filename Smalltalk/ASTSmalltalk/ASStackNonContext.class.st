"
All the things on the stack with real content
"
Class {
	#name : 'ASStackNonContext',
	#superclass : 'ASStackNonEmpty',
	#instVars : [
		'outputName',
		'data'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASStackNonContext >> data [

	^ data
]

{ #category : 'accessing' }
ASStackNonContext >> data: anObject [

	anObject isFlowSource ifFalse: [ ^ self error: 'must be source' ].
	data := anObject
]

{ #category : 'evaluating' }
ASStackNonContext >> dataClass [

	^ data dataClass
]

{ #category : 'output' }
ASStackNonContext >> getNameOn: anASLLMethodOutput [

	^ data getNameFor: self on: anASLLMethodOutput
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> id [
	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASStackNonContext >> makeStackTheSource [

	data := ASFlowOnDemand from: data.
	^ next makeStackTheSource
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> markReferenced [
	^ self subclassResponsibility
]

{ #category : 'operations' }
ASStackNonContext >> mergeWith: aStackElement [

	^ self subclassResponsibility
]

{ #category : 'stack' }
ASStackNonContext >> offset: anInteger to: aStackElement [

	self == aStackElement ifTrue: [ ^ anInteger ].
	^ next offset: anInteger+1 to: aStackElement
]

{ #category : 'API' }
ASStackNonContext >> replicate: aStack [

	^ (self class fromStack: aStack) data: data
]
