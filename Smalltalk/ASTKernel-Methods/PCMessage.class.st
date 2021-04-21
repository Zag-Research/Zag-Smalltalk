Class {
	#name : #PCMessage,
	#superclass : #PCObject,
	#instVars : [
		'selector',
		'arguments',
		'lookupClass'
	],
	#category : #'ASTKernel-Methods'
}

{ #category : #'as yet unclassified' }
PCMessage >> arguments [
	"Answer the message arguments array."

	^ arguments
]

{ #category : #'as yet unclassified' }
PCMessage >> lookupClass [
	"Answer the message lookupClass."

	^ lookupClass
]

{ #category : #'as yet unclassified' }
PCMessage >> printOn: aStream [
	"Refer to the comment in Object|printOn:."

	super printOn: aStream.
	aStream
		nextPutAll: ' selector: ' , selector printString;
		nextPutAll: ' args: ' , arguments printString
]

{ #category : #'as yet unclassified' }
PCMessage >> selector [
	"Answer the message selector."

	^ selector
]

{ #category : #'as yet unclassified' }
PCMessage >> sentTo: anObject [
	"Answer the result of sending this message to the given object."

	lookupClass == nil
		ifTrue: [ ^ anObject perform: selector withArguments: arguments ]
		ifFalse: [ ^ anObject
				perform: selector
				withArguments: arguments
				inSuperclass: lookupClass ]
]
