"
Clean up stack and return self
"
Class {
	#name : #ASCReturnSelf,
	#superclass : #ASCOperations,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #'block handling' }
ASCReturnSelf >> manifestParameters: stack [
]

{ #category : #accessing }
ASCReturnSelf >> next [

	^ nil
]

{ #category : #'code generation' }
ASCReturnSelf >> outputTo: output [

	^ output visitReturnSelf
]

{ #category : #'instance creation' }
ASCReturnSelf >> with: ignored [


]
