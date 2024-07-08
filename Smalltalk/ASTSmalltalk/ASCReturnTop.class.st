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
ASCReturnTop >> resultStack: aStack [

	stack := aStack.
	^ aStack drop
]
