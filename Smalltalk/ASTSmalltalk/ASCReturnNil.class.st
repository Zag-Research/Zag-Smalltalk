"
Clean up stack and return nil
"
Class {
	#name : #ASCReturnNil,
	#superclass : #ASCOperations,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #'block handling' }
ASCReturnNil >> manifestParameters: stack [
]

{ #category : #accessing }
ASCReturnNil >> next [

	^ nil
]

{ #category : #'code generation' }
ASCReturnNil >> outputTo: output [

	^ output visitReturnNil
]

{ #category : #'instance creation' }
ASCReturnNil >> with: ignored [


]
