"
I have the common methods for non-empty stack elements
"
Class {
	#name : 'ASStackElement',
	#superclass : 'ASStack',
	#instVars : [
		'offset',
		'next',
		'location'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'enumerating' }
ASStackElement >> addToCollection: aCollection [

	aCollection addLast: self.
	^ next addToCollection: aCollection
]

{ #category : 'accessing' }
ASStackElement >> at: aSymbol [

	^ next at: aSymbol
]

{ #category : 'accessing' }
ASStackElement >> context [

	^ next context
]

{ #category : 'as yet unclassified' }
ASStackElement >> data [

	^ location stackValue
]

{ #category : 'accessing' }
ASStackElement >> data: aStackValue [

	(aStackValue isKindOf: AStackValue) ifFalse: [
		self error: 'must be a AStackValue' ].
	location stackValue: aStackValue
]

{ #category : 'evaluating' }
ASStackElement >> dataClass [

	^ location dataClass
]

{ #category : 'displaying' }
ASStackElement >> displayStringOn: stream [

	^ self subclassResponsibility
]

{ #category : 'enumerating' }
ASStackElement >> do: aBlock [

	aBlock value: self.
	^ next do: aBlock
]

{ #category : 'instance creation' }
ASStackElement >> fromStack: anASStack [

	offset := anASStack offset + 1.
	next := anASStack
]

{ #category : 'output' }
ASStackElement >> getNameOn: anASLLMethodOutput [

	^ location getNameFor: self on: anASLLMethodOutput
]

{ #category : 'testing' }
ASStackElement >> hasContext [

	^ next hasContext
]

{ #category : 'as yet unclassified' }
ASStackElement >> id [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
ASStackElement >> initialize [

	super initialize.
	location := AStackLocation new
]

{ #category : 'API' }
ASStackElement >> intoContext: anInteger [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ASStackElement >> isEmpty [

	^ false
]

{ #category : 'testing' }
ASStackElement >> isInContext [

	^ false
]

{ #category : 'testing' }
ASStackElement >> isLocal [

	^ false
]

{ #category : 'API' }
ASStackElement >> linkContext: aBlock [

	^ self replicate: (next linkContext: aBlock)
]

{ #category : 'accessing' }
ASStackElement >> location [
	^ location
]

{ #category : 'accessing' }
ASStackElement >> location: anAStackLocation [

	location := anAStackLocation
]

{ #category : 'data flow instructions' }
ASStackElement >> makeStackTheSource [

	location := ASFlowOnDemand from: location.
	^ next makeStackTheSource
]

{ #category : 'data flow instructions' }
ASStackElement >> markReferenced [

	^ location markReferenced
]

{ #category : 'operations' }
ASStackElement >> mergeWith: aStackElement [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASStackElement >> newElementFor: aBasicBlock [

	^ self replicate: (next newElementFor: aBasicBlock)
]

{ #category : 'accessing' }
ASStackElement >> next [
	^ next
]

{ #category : 'accessing' }
ASStackElement >> offset [

	^ offset
]

{ #category : 'stack' }
ASStackElement >> offset: anInteger to: aStackElement [

	self == aStackElement ifTrue: [ ^ anInteger ].
	^ next offset: anInteger+1 to: aStackElement
]

{ #category : 'as yet unclassified' }
ASStackElement >> offsetToSelf: anInteger [

	^ next offsetToSelf: anInteger + 1
]

{ #category : 'stack' }
ASStackElement >> replicate [

	^ (self replicate: next replicate) data: location
]

{ #category : 'API' }
ASStackElement >> replicate: aStack [

	^ (self class fromStack: aStack) location: location
]

{ #category : 'as yet unclassified' }
ASStackElement >> stackValue [

	^ location stackValue
]

{ #category : 'enumerating' }
ASStackElement >> with: other do: aBlock [

	aBlock value: self value: other.
	^ next with: other next do: aBlock
]
