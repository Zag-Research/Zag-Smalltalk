"
I represent an empty stack
"
Class {
	#name : 'ASEmptyStack',
	#superclass : 'ASStack',
	#instVars : [
		'nonlocalNameLookupObject',
		'blocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASEmptyStack >> blocks: aCollection [
	aCollection isCollection ifFalse: [ self error: 'blocks must be a collection' ].
	blocks := aCollection
]

{ #category : 'accessing' }
ASEmptyStack >> depth [ 
^ 0
]

{ #category : 'accessing' }
ASEmptyStack >> find: aSymbol [

	| stack |
	stack := ASStackNamedIndirect new
		         from: self;
		         name: aSymbol.
	stack top: (nonlocalNameLookupObject find: aSymbol).
	^ stack
	"
	^ nonlocalNameLookupObject find: aSymbol"
]

{ #category : 'initialization' }
ASEmptyStack >> initialize [
	super initialize.
	blocks := #()
]

{ #category : 'instance initialization' }
ASEmptyStack >> nonlocalNameLookupObject: anASCodeGenerator [

	nonlocalNameLookupObject := anASCodeGenerator
]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger to: anObject [
	^anInteger 
]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

	blocks do: [ :block |
		| index |
		index := block indexOf: anObject.
		index > 0 ifTrue: [ ^ blockOther value: index ] ].

	^ self error: 'something bad happend'
]

{ #category : 'initialization' }
ASEmptyStack >> requiredContextDo: x [
]
