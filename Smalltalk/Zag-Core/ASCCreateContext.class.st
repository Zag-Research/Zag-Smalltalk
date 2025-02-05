"
I create a context on the current stack.

There may be values above where the context goes, which will be saved.
"
Class {
	#name : 'ASCCreateContext',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'context'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'callgraph' }
ASCCreateContext >> dotStringOn: aStream [

	aStream << 'createContext'
]

{ #category : 'stack' }
ASCCreateContext >> resultStack: anASStack [

	context := anASStack context.
	stack := anASStack.
	^ anASStack
]
