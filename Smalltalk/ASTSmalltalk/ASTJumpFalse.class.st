Class {
	#name : #ASTJumpFalse,
	#superclass : #ASTJump,
	#category : #'ASTSmalltalk-IR'
}

{ #category : #'as yet unclassified' }
ASTJumpFalse >> blah [
	^ super foo
]

{ #category : #'reflective operations' }
ASTJumpFalse >> doesNotUnderstand: message [
	^ 42
]

{ #category : #thread }
ASTJumpFalse >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.ifFalse,'.
	destination threadFormatOn: threadPrinter.
	^ self otherwise
]
