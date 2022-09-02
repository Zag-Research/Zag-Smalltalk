Class {
	#name : #ASTJumpFalse,
	#superclass : #ASTJump,
	#category : #'ASTSmalltalk-IR'
}

{ #category : #thread }
ASTJumpFalse >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.ifFalse,'.
	destination threadFormatOn: threadPrinter.
	^ self otherwise
]
