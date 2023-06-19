Class {
	#name : #ASInstruction,
	#superclass : #Object,
	#category : #'ASTSmalltalk-Image'
}

{ #category : #visiting }
ASInstruction >> visitPushReceiver: anIRPushReceiver [ 
	^ ASInstruction new
]
