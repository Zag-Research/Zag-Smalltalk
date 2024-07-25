"
I have the common methods for non-empty stack elements
"
Class {
	#name : 'ASStackNonEmpty',
	#superclass : 'ASStack',
	#instVars : [
		'offset',
		'next',
		'data'
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

	^ data
]

{ #category : 'accessing' }
ASStackNonEmpty >> data: anObject [

	anObject isFlowSource ifFalse: [ ^ self error: 'must be source' ].
	data := anObject
]

{ #category : 'evaluating' }
ASStackNonEmpty >> dataClass [

	^ data dataClass
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

	next := anASStack.
	offset := anASStack offset - 1
]

{ #category : 'output' }
ASStackNonEmpty >> getNameOn: anASLLMethodOutput [

	^ data getNameFor: self on: anASLLMethodOutput
]

{ #category : 'testing' }
ASStackNonEmpty >> hasContext [

	^ next hasContext
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> id [
	^ self subclassResponsibility
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

{ #category : 'data flow instructions' }
ASStackNonEmpty >> makeStackTheSource [

	data := ASFlowOnDemand from: data.
	^ next makeStackTheSource
]

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> markReferenced [
	^ self subclassResponsibility
]

{ #category : 'operations' }
ASStackNonEmpty >> mergeWith: aStackElement [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASStackNonEmpty >> newElementFor: aBasicBlock [ 
	^ self subclassResponsibility
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

	^ (self replicate: next replicate) data: data
]

{ #category : 'API' }
ASStackNonEmpty >> replicate: aStack [

	^ (self class fromStack: aStack) data: data
]

{ #category : 'enumerating' }
ASStackNonEmpty >> with: other do: aBlock [

	aBlock value: self value: other.
	^ next with: other next do: aBlock
]
