"
I remember to push local variable
"
Class {
	#name : #ASCPushLocal,
	#superclass : #ASCLocalOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCPushLocal >> outputTo: anOutput [ 
	anOutput putEmbedded: #pushLocal;putInteger: localNumber 
]

{ #category : #visitor }
ASCPushLocal >> zigAccept: aVisitor [ 
	aVisitor visitPushLocal: localNumber 
]
