"
I have the common methods for non-empty stack elements
"
Class {
	#name : 'ASStackNonEmpty',
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

{ #category : 'instance creation' }
ASStackNonEmpty class >> fromStack: anASStack [

	^ self new fromStack: anASStack
]

{ #category : 'enumerating' }
ASStackNonEmpty >> addToCollection: aCollection [

	aCollection addLast: self.
	^ next addToCollection: aCollection
]

{ #category : 'accessing' }
ASStackNonEmpty >> at: aSymbol [

	^ next at: aSymbol
]

{ #category : 'accessing' }
ASStackNonEmpty >> at: aSymbol put: anObject [

	| element |
	element := self at: aSymbol.
	element data: (element data value: anObject)
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> data [

	^ location stackValue
]

{ #category : 'accessing' }
ASStackNonEmpty >> data: aStackValue [

	(aStackValue isKindOf: AStackValue) ifFalse: [
		self error: 'must be a AStackValue' ].
	location stackValue: aStackValue
]

{ #category : 'evaluating' }
ASStackNonEmpty >> dataClass [

	^ location dataClass
]

{ #category : 'displaying' }
ASStackNonEmpty >> displayStringOn: stream [

	^ self subclassResponsibility
]

{ #category : 'enumerating' }
ASStackNonEmpty >> do: aBlock [

	aBlock value: self.
	^ next do: aBlock
]

{ #category : 'instance creation' }
ASStackNonEmpty >> fromStack: anASStack [

	next := anASStack
]

{ #category : 'output' }
ASStackNonEmpty >> getNameOn: anASLLMethodOutput [

	^ location getNameFor: self on: anASLLMethodOutput
]

{ #category : 'testing' }
ASStackNonEmpty >> hasContext [

	^ next hasContext
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> id [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
ASStackNonEmpty >> initialize [

	super initialize.
	location := AStackLocation new
]

{ #category : 'API' }
ASStackNonEmpty >> intoContext: anInteger [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ASStackNonEmpty >> isEmpty [

	^ false
]

{ #category : 'testing' }
ASStackNonEmpty >> isInContext [

	^ false
]

{ #category : 'API' }
ASStackNonEmpty >> linkContext: aBlock [

	^ self replicate: (next linkContext: aBlock)
]

{ #category : 'accessing' }
ASStackNonEmpty >> location [
	^ location
]

{ #category : 'accessing' }
ASStackNonEmpty >> location: anAStackLocation [

	location := anAStackLocation
]

{ #category : 'data flow instructions' }
ASStackNonEmpty >> makeStackTheSource [

	location := ASFlowOnDemand from: location.
	^ next makeStackTheSource
]

{ #category : 'data flow instructions' }
ASStackNonEmpty >> markReferenced [

	^ location markReferenced
]

{ #category : 'operations' }
ASStackNonEmpty >> mergeWith: aStackElement [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASStackNonEmpty >> newElementFor: aBasicBlock [

	^ self replicate: (next newElementFor: aBasicBlock)
]

{ #category : 'accessing' }
ASStackNonEmpty >> next [
	^ next
]

{ #category : 'accessing' }
ASStackNonEmpty >> offset [

	^ offset
]

{ #category : 'stack' }
ASStackNonEmpty >> offset: anInteger to: aStackElement [

	self == aStackElement ifTrue: [ ^ anInteger ].
	^ next offset: anInteger+1 to: aStackElement
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> offsetToSelf: anInteger [

	^ next offsetToSelf: anInteger + 1
]

{ #category : 'stack' }
ASStackNonEmpty >> replicate [

	^ (self replicate: next replicate) data: location
]

{ #category : 'API' }
ASStackNonEmpty >> replicate: aStack [

	^ (self class fromStack: aStack) location: location
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> stackValue [

	^ location stackValue
]

{ #category : 'enumerating' }
ASStackNonEmpty >> with: other do: aBlock [

	aBlock value: self value: other.
	^ next with: other next do: aBlock
]
