"
I represent a return
"
Class {
	#name : 'ASCReturnTop',
	#superclass : 'ASCReturn',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'output' }
ASCReturnTop >> getTop: anASLLMethodOutput [

	^ stack getNameOn: anASLLMethodOutput 
]

{ #category : 'block handling' }
ASCReturnTop >> manifestParameters [

	^ stack makeManifestFor: 1
]

{ #category : 'data flow instructions' }
ASCReturnTop >> markReferences [

	stack markReferenced
]

{ #category : 'code generation' }
ASCReturnTop >> outputTo: output [

	^ output visitReturnTop: self
]

{ #category : 'stack' }
ASCReturnTop >> resultStack: aStack basicBlock: aBasicBlock [

	| stackElement |
	stack := aStack.
	stackElement := aStack at: #self.
	^ (ASStackTemporary fromStack: stackElement next) data: aStack data
]
