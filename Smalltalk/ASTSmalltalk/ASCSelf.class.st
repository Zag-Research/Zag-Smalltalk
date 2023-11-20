"
I am the self for the method (not the hidden self for blocks)
"
Class {
	#name : #ASCSelf,
	#superclass : #ASCParameter,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCSelf >> name [ 
	^ #self
]

{ #category : #'code generation' }
ASCSelf >> outputTo: anOutput [

	^ anOutput visitPushLocal: offset
]
