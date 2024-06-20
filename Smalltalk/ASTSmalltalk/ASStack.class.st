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

{ #category : 'API' }
ASStack >> initLocalNamed: name [

	^ (ASLocalNamed new fromStack: self) name: name
]

{ #category : 'API' }
ASStack >> initParameterNamed: name [

	^ (ASParameterNamed fromStack: self) name: name
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
	newStack := ASStackNamed fromStack: self.
	newStack value: anObject.
	^ newStack
]

{ #category : 'API' }
ASStack >> pushSelf [

	^ ASStackSelf fromStack: self
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]
