"
I am used to copy top values from the stack to an enclosing space on the stack and clean up the stack to that point.
"
Class {
	#name : 'ASCCopyVariablesCleanStack',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'target',
		'source'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCCopyVariablesCleanStack >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: self numberToCopy;
		nextPutAll: ' from ';
		print: source;
		nextPutAll: ' to ';
		print: target
]

{ #category : 'printing' }
ASCCopyVariablesCleanStack >> numberToCopy [
		^ target offsetToSelf
]

{ #category : 'code generation' }
ASCCopyVariablesCleanStack >> outputTo: aCodeEmitter [

	^ aCodeEmitter
		  visitCopy: self numberToCopy
		  variableCleanStackTo: target
		  from: source
]

{ #category : 'stack' }
ASCCopyVariablesCleanStack >> resultStack: aStack basicBlock: anObject [

	stack := aStack.
	^ target
]

{ #category : 'code generation' }
ASCCopyVariablesCleanStack >> target: targetElement source: sourceElement [

	target := targetElement.
	source := sourceElement
]

{ #category : 'instance creation' }
ASCCopyVariablesCleanStack >> with: anObject [

]
