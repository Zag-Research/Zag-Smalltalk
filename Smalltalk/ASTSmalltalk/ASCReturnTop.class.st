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

{ #category : 'as yet unclassified' }
ASCReturnTop >> manifestParameters: stack [

	^ stack top makeManifest
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

	^ output visitReturnTop
]

{ #category : 'instance creation' }
ASCReturnTop >> with: aStack [

	stack := aStack
]
