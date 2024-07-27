"
All the things on the stack with real content
"
Class {
	#name : 'ASStackNonContext',
	#superclass : 'ASStackNonEmpty',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'output' }
ASStackNonContext >> getNameOn: anASLLMethodOutput [

	^ data getNameFor: self on: anASLLMethodOutput
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> id [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASStackNonContext >> linkContext: aBlock [

	^ self replicate: (next linkContext: aBlock)
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

{ #category : 'stack' }
ASStackNonContext >> replicate [

	^ (self replicate: next replicate) data: data
]

{ #category : 'API' }
ASStackNonContext >> replicate: aStack [

	^ (self class fromStack: aStack) data: data
]
