"
I reperesent an empty stack of values within a method.
"
Class {
	#name : 'ASStack',
	#superclass : 'Object',
	#instVars : [
		'rest'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASStack class >> fromStack: anASStack [

	^ self new fromStack: anASStack
]

{ #category : 'accessing' }
ASStack >> at: aSymbol [

	^ (self find: aSymbol) top
]

{ #category : 'accessing' }
ASStack >> drop [
	^ rest
]

{ #category : 'removing' }
ASStack >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : 'accessing' }
ASStack >> find: aSymbol [
self subclassResponsibility 
]

{ #category : 'instance creation' }
ASStack >> fromStack: anASStack [

	rest := anASStack
]

{ #category : 'testing' }
ASStack >> isEmpty [

	^ true
]

{ #category : 'adding' }
ASStack >> nameStack: aCollection [ 

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

{ #category : 'as yet unclassified' }
ASStack >> offsetToSelf: anInteger [ 

	^ rest offsetToSelf: anInteger + 1
]

{ #category : 'adding' }
ASStack >> push: anObject [

	| newStack |
	newStack := ASStackElement fromStack: self.
	newStack top: anObject. " (ASCClassValue new init: anObject class)."
	^ newStack
]

{ #category : 'adding' }
ASStack >> push: aSymbol on: aStack [

	^ self shouldBeImplemented
]

{ #category : 'API' }
ASStack >> pushLocalNamed: name [

	^ (ASLocalNamed new fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> pushParameterNamed: name [

	^ ((#self = name
		    ifTrue: [ ASStackSelf ]
		    ifFalse: [ ASStackNamed ]) fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> pushPlaceholderContext [ 
	^ ASStackContextPlaceholder new fromStack: self
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]
