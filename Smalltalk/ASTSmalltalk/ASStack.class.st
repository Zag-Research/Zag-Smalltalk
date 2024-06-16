"
I reperesent an empty stack of values within a method.
"
Class {
	#name : 'ASStack',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'removing' }
ASStack >> drop: count [

	count = 0 ifTrue: [ ^ self ].
	self error
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

{ #category : 'accessing' }
ASStack >> offset [

	^ 0
]

{ #category : 'adding' }
ASStack >> push: anObject [

	| newStack |
	newStack := ASStackElement fromStack: self.
	newStack top: anObject. 
	^ newStack
]

{ #category : 'API' }
ASStack >> pushLocalNamed: name basicBlock: anObject [

	^ (ASLocalNamed new fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> pushParameterNamed: name [

	^ (ASStackNamed fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> pushSelf [

	^ ASStackSelf new fromStack: self
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]
