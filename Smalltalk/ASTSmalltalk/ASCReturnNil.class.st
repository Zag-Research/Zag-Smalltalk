"
Clean up stack and return nil
"
Class {
	#name : #ASCReturnNil,
	#superclass : #ASCOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCReturnNil >> outputTo: output [

	^ output visitReturnNil
]

{ #category : #'instance creation' }
ASCReturnNil >> with: ignored [


]