"
I represent an instance variable
"
Class {
	#name : 'ASCInstanceVariable',
	#superclass : 'ASCVariable',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'affecting' }
ASCInstanceVariable >> pop: output [	^ output visitPopInstance: self
]

{ #category : 'affecting' }
ASCInstanceVariable >> push: output [

	^ output visitPushInstance: self
]

{ #category : 'affecting' }
ASCInstanceVariable >> store: output [

	^ output visitStoreInstance: self
]
