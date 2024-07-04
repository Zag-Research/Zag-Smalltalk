"
I represent a return
"
Class {
	#name : 'ASCReturnTop',
	#superclass : 'ASCStackReferencingOperations',
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

{ #category : 'accessing' }
ASCReturnTop >> next [
	^ nil
]

{ #category : 'code generation' }
ASCReturnTop >> outputTo: output [

	^ output visitReturnTop: self
]

{ #category : 'stack' }
ASCReturnTop >> resultStack: anASStack [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCReturnTop >> with: aStack [

	stack := aStack
]
