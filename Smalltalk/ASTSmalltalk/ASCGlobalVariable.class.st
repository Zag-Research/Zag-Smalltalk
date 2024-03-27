"
I represent a global (most likely a class)
"
Class {
	#name : #ASCGlobalVariable,
	#superclass : #ASCVariable,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #compiling }
ASCGlobalVariable >> tell: aCompiler youAreSent: aSymbol [

	aCompiler sent: aSymbol toGlobal: name
]
