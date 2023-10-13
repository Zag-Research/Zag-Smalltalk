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
ASStack >> depth [ 
^ 0
]

{ #category : #accessing }
ASStack >> find: aSymbol [
self subclassResponsibility 
]

{ #category : #API }
ASStack >> findContext [
	^ self subclassResponsibility
]

{ #category : #testing }
ASStack >> isEmpty [

	^ true
]

{ #category : #API }
ASStack >> offset: ignored to: unused [
self subclassResponsibility 
]

{ #category : #API }
ASStack >> offsetTo: aSymbol [
^ self offset: 0 to: aSymbol
]

{ #category : #adding }
ASStack >> push: anObject [

	| newStack |
	newStack := ASStackElement from: self.
	newStack top: anObject.
	^ newStack 
]

{ #category : #API }
ASStack >> pushContext [ 
	^ ASStackContext new from: self
]

{ #category : #API }
ASStack >> stackNames: aCollection [

	aCollection ifEmpty: [ ^ self ].
	^ ASStackNamed new
		  from: (self stackNames: aCollection allButFirst);
		  name: aCollection first
]
