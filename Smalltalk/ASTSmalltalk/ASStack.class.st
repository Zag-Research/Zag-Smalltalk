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

{ #category : 'enumerating' }
ASStack >> do: ignored [
]

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

{ #category : 'API' }
ASStack >> initSelf [

	^ ASStackSelf fromStack: self
]

{ #category : 'API' }
ASStack >> intoContext: anInteger [ 

]

{ #category : 'testing' }
ASStack >> isEmpty [

	^ true
]

{ #category : 'adding' }
ASStack >> newElementFor: notNeeded [
]

{ #category : 'accessing' }
ASStack >> offset [

	^ 0
]

{ #category : 'adding' }
ASStack >> push: anObject [

	^ (ASStackTemporary fromStack: self) value: anObject; source: ASFlowNew new
]

{ #category : 'API' }
ASStack >> pushUnknown [
	^ self push: ASCUnknownValue new
]

{ #category : 'data flow instructions' }
ASStack >> simplifyDependenciesFor: anASCMethodBlock [

]
