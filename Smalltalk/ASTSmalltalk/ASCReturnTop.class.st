"
I represent a return
"
Class {
	#name : #ASCReturnTop,
	#superclass : #ASCOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'block handling' }
ASCReturnTop >> manifestParameters: stack [

	^ stack top makeManifest
]

{ #category : #accessing }
ASCReturnTop >> next [
	^ nil
]

{ #category : #'code generation' }
ASCReturnTop >> outputTo: output [

	^ output visitReturnTop
]

{ #category : #'instance creation' }
ASCReturnTop >> with: ignored [


]
