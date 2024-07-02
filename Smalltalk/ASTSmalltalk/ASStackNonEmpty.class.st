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

{ #category : 'accessing' }
ASStackNonEmpty >> at: aSymbol [

	^ next at: aSymbol
]

{ #category : 'accessing' }
ASStackNonEmpty >> at: aSymbol put: anObject [

	^ (self at: aSymbol) value: anObject
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

{ #category : 'enumerating' }
ASStackNonEmpty >> with: other do: aBlock [

	aBlock value: self value: other.
	^ next with: other next do: aBlock
]
