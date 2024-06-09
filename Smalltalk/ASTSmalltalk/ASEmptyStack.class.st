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
ASEmptyStack >> depth [ 
^ 0
]

{ #category : 'accessing' }
ASEmptyStack >> find: aSymbol [

	| stack |
	stack := ASStackNamedIndirect new
		         from: self;
		         name: aSymbol.
	stack top: (nonlocalNameLookupObject scopeFind: aSymbol).
	^ stack
	"
	^ nonlocalNameLookupObject find: aSymbol"
]

{ #category : 'API' }
ASEmptyStack >> makeContextRequired [

	^ ASStackContext fromStack: self
]

{ #category : 'instance initialization' }
ASEmptyStack >> nonlocalNameLookupObject: anASCodeGenerator [

	nonlocalNameLookupObject := anASCodeGenerator
]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger namedContext: blockLocal stack: blockStack [

]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger to: anObject [
	^anInteger 
]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [

	^ self error: 'something bad happend'
]

{ #category : 'initialization' }
ASEmptyStack >> requiredContextDo: x [
]

{ #category : 'initialization' }
ASEmptyStack >> reset [

	^ self
]
