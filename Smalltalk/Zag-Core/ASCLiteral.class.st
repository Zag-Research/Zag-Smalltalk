"
I remember literal values to output.
"
Class {
	#name : 'ASCLiteral',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'literal'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'callgraph' }
ASCLiteral >> dotStringOn: aStream [

	aStream
		<< 'push literal ';
		print: literal
]

{ #category : 'printing' }
ASCLiteral >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: literal.
	aStream << $)
]

{ #category : 'code generation' }
ASCLiteral >> outputTo: output [ 
	^ output visitPushLiteral: literal
]

{ #category : 'stack' }
ASCLiteral >> resultStack: anASStack [

	stack := anASStack.
	^ self createsTemporary: anASStack data:
		  (AStackValue fromValue: literal)
]

{ #category : 'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
