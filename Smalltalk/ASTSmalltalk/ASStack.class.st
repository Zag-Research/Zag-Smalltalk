"
I reperesent an empty stack of values within a method.
"
Class {
	#name : 'ASStack',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASStack >> at: aSymbol [

	^ (self find: aSymbol) top
]

{ #category : 'accessing' }
ASStack >> find: aSymbol [
self subclassResponsibility 
]

{ #category : 'testing' }
ASStack >> isEmpty [

	^ true
]

{ #category : 'API' }
ASStack >> namedContext: blockLocal stack: blockStack [
	^ self
		  offset: 0
		  namedContext: blockLocal
		  stack: blockStack
]

{ #category : 'API' }
ASStack >> offset: anInteger namedContext: blockLocal stack: blockStack [

	self subclassResponsibility
]

{ #category : 'API' }
ASStack >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [

self subclassResponsibility 
]

{ #category : 'API' }
ASStack >> offsetOf: aSymbol context: blockLocal stack: blockStack other: blockOther [

	^ self
		  offset: 0
		  to: aSymbol
		  context: blockLocal
		  stack: blockStack
		  other: blockOther 
]

{ #category : 'adding' }
ASStack >> push: anObject [

	| newStack |
	newStack := ASStackElement from: self.
	newStack top: anObject. " (ASCClassValue new init: anObject class)."
	^ newStack
]

{ #category : 'adding' }
ASStack >> push: aSymbol on: aStack [

]

{ #category : 'API' }
ASStack >> pushLocalNamed: name [

	^ (ASLocalNamed new from: self) name: name
]

{ #category : 'API' }
ASStack >> pushParameterNamed: name [

	^ (ASStackNamed new from: self) name: name
]

{ #category : 'API' }
ASStack >> pushPlaceholderContext [ 
	^ ASStackContextPlaceholder new from: self
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]
