"
I represent a methods empty stack.
"
Class {
	#name : 'ASMethodEmptyStack',
	#superclass : 'ASEmptyStack',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASMethodEmptyStack >> find: aSymbol [

	^ nonlocalNameLookupObject lookup: aSymbol
]
