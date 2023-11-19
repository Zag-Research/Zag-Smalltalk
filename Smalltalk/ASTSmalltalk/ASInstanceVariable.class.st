"
I represent an instance variable
"
Class {
	#name : 'ASInstanceVariable',
	#superclass : 'ASVariable',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'visiting' }
ASInstanceVariable >> acceptVisitor: aVisitor [

	^ aVisitor visitInstance: self
]

{ #category : 'affecting' }
ASInstanceVariable >> pop: output [	^ output visitPopInstance: self
]

{ #category : 'affecting' }
ASInstanceVariable >> push: output [

	^ output visitPushInstance: self
]

{ #category : 'affecting' }
ASInstanceVariable >> store: output [

	^ output visitStoreInstance: self
]
