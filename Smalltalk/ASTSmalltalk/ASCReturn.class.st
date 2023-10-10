"
I represent a return
"
Class {
	#name : #ASCReturn,
	#superclass : #ASCOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCReturn >> outputTo: output [

	output doReturn
]

{ #category : #'instance creation' }
ASCReturn >> with: ignored [


]

{ #category : #visitor }
ASCReturn >> zigAccept: aVisitor [

	^ aVisitor visitReturn
]
