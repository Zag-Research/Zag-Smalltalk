"
I reperesent an empty stack of values within a method.
"
Class {
	#name : #ASStack,
	#superclass : #Object,
	#instVars : [
		'nonlocalNameLookupObject'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASStack >> at: aSymbol [

	^ (self find: aSymbol) top
]

{ #category : #accessing }
ASStack >> depth [ 
^ 0
]

{ #category : #accessing }
ASStack >> find: aSymbol [
self subclassResponsibility 
]

{ #category : #testing }
ASStack >> isEmpty [

	^ true
]

{ #category : #'instance initialization' }
ASStack >> nonlocalNameLookupObject: anASCodeGenerator [ 
	nonlocalNameLookupObject := anASCodeGenerator 
]

{ #category : #API }
ASStack >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

	^ nonlocalNameLookupObject lookupName: anObject block: blockOther 
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
