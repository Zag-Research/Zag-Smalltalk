Class {
	#name : 'ASCCopyVariable',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'target',
		'source',
		'offset'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCCopyVariable >> target: targetElement source: sourceElement [

	target := targetElement.
	source := sourceElement
]
