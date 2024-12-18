"
I represent an instance variable
"
Class {
	#name : 'ASCInstanceVariable',
	#superclass : 'ASCVariable',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
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
