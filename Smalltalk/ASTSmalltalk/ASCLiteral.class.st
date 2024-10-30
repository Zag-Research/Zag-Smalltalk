"
I remember literal values to output.
"
Class {
	#name : 'ASCLiteral',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'literal'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
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

{ #category : 'Collection API' }
ASCLiteral >> interpretFor: anInterpreter [

	^ anInterpreter push: literal
]

{ #category : 'code generation' }
ASCLiteral >> outputTo: output [ 
	^ output visitPushLiteral: literal
]

{ #category : 'stack' }
ASCLiteral >> resultStack: anASStack basicBlock: aBasicBlock [

	^ self creates: (anASStack pushDataAsTemporary:
		             (AStackValue fromValue: literal))
]

{ #category : 'instance creation' }
ASCLiteral >> with: anObject [

	literal := anObject 
]
