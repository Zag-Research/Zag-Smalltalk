"
I remember local pops
"
Class {
	#name : #ASCPopLocal,
	#superclass : #ASCLocalOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCPopLocal >> outputTo: anOutput [ 
	anOutput putEmbedded: #popLocal
]

{ #category : #visitor }
ASCPopLocal >> zigAccept: aVisitor [ 
	^ aVisitor visitPopLocal: localNumber 
]
