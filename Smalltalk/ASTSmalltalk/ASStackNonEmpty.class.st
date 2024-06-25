"
I have the common methods for non-empty stack elements
"
Class {
	#name : 'ASStackNonEmpty',
	#superclass : 'ASStack',
	#instVars : [
		'rest',
		'offset'
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

	^ rest at: aSymbol
]

{ #category : 'accessing' }
ASStackNonEmpty >> at: aSymbol put: anObject [

	^ (self at: aSymbol) value: anObject
]

{ #category : 'enumerating' }
ASStackNonEmpty >> do: aBlock [

	aBlock value: self.
	^ rest do: aBlock
]

{ #category : 'instance creation' }
ASStackNonEmpty >> fromStack: anASStack [

	rest := anASStack.
	offset := anASStack offset - 1
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
ASStackNonEmpty >> offset [

	^ offset
]
