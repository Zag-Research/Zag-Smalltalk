"
I have the common methods for non-empty stack elements
"
Class {
	#name : 'ASStackNonEmpty',
	#superclass : 'ASStack',
	#instVars : [
		'offset',
		'next'
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

{ #category : 'testing' }
ASStackNonEmpty >> hasContext [

	^ next hasContext
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
ASStackNonEmpty >> makeContextRequired: aFullBlockClosure [ 
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

{ #category : 'as yet unclassified' }
ASStackNonEmpty >> offsetToSelf: anInteger [

	^ next offsetToSelf: anInteger + 1
]

{ #category : 'enumerating' }
ASStackNonEmpty >> with: other do: aBlock [

	aBlock value: self value: other.
	^ next with: other next do: aBlock
]
