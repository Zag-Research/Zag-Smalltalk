Class {
	#name : 'ASTJumpTrue',
	#superclass : 'ASTJump',
	#category : 'ASTSmalltalk-IR',
	#package : 'ASTSmalltalk',
	#tag : 'IR'
}

{ #category : 'thread' }
ASTJumpTrue >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.ifTrue,'.
	destination threadFormatOn: threadPrinter.
	^ self otherwise
]
