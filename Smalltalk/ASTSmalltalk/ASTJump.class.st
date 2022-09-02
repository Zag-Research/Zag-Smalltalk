Class {
	#name : #ASTJump,
	#superclass : #Object,
	#instVars : [
		'destination'
	],
	#category : #'ASTSmalltalk-IR'
}

{ #category : #initializing }
ASTJump >> from: anIRJump [ 
	destination := anIRJump destination
]

{ #category : #thread }
ASTJump >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.branch,'.
	destination threadFormatOn: threadPrinter.
	^ self otherwise
]
