"
I create a context on the current stack.

There may be values above where the context goes, which will be saved.
"
Class {
	#name : 'ASCCreateContext',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'stack' }
ASCCreateContext >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ anASStack
]
