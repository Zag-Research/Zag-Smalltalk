"
I reperesent an empty stack of values within a method.
"
Class {
	#name : #ASStack,
	#superclass : #Object,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASStack >> at: aSymbol [

	^ (self find: aSymbol) top
]

{ #category : #accessing }
ASStack >> find: aSymbol [
self subclassResponsibility 
]

{ #category : #testing }
ASStack >> isEmpty [

	^ true
]

{ #category : #API }
ASStack >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

self subclassResponsibility 
]

{ #category : #API }
ASStack >> offsetOf: aSymbol local: blockLocal stack: blockStack other: blockOther [

	^ self
		  offset: 0
		  to: aSymbol
		  local: blockLocal
		  stack: blockStack
		  other: nil
]

{ #category : #adding }
ASStack >> push: anObject [

	| newStack |
	newStack := ASStackElement from: self.
	newStack top: anObject.
	^ newStack 
]

{ #category : #API }
ASStack >> pushPlaceholderContext [ 
	^ ASStackContextPlaceholder new from: self
]

{ #category : #API }
ASStack >> stackNames: aCollection [

	aCollection ifEmpty: [ ^ self ].
	^ ASStackNamed new
		  from: (self stackNames: aCollection allButFirst);
		  name: aCollection first
]
