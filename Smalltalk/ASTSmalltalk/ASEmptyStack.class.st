"
I represent an empty stack
"
Class {
	#name : 'ASEmptyStack',
	#superclass : 'ASStack',
	#instVars : [
		'nonlocalNameLookupObject'
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
	stack top: (nonlocalNameLookupObject find: aSymbol).
	^ stack
	"
	^ nonlocalNameLookupObject find: aSymbol"
]

{ #category : 'instance initialization' }
ASEmptyStack >> nonlocalNameLookupObject: anASCodeGenerator [

	nonlocalNameLookupObject := anASCodeGenerator
]

{ #category : 'API' }
ASEmptyStack >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [
	^ nonlocalNameLookupObject lookupName: anObject block: blockOther
]
