"
I rempresent a stack context.

I may,  or may not be reified in a particular method
"
Class {
	#name : 'ASStackContextPlaceholder',
	#superclass : 'ASStack',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASStackContextPlaceholder >> depth [
	^ rest depth
]

{ #category : 'removing' }
ASStackContextPlaceholder >> drop: count [

	count = 0 ifTrue: [ ^ self ].
	^ self error: 'stack underflow'
]

{ #category : 'accessing' }
ASStackContextPlaceholder >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : 'adding' }
ASStackContextPlaceholder >> importedSource: anInteger [

	^ self class fromStack: (rest importedSource: anInteger)
]

{ #category : 'API' }
ASStackContextPlaceholder >> makeContextRequired [
	^ ASStackContext from: rest
]

{ #category : 'API' }
ASStackContextPlaceholder >> offset: anInteger namedContext: blockLocal stack: blockStack [

	^ rest
		  offset: anInteger
		  namedContext: blockLocal
		  stack: blockStack
]

{ #category : 'API' }
ASStackContextPlaceholder >> offset: anInteger to: anObject [ 

	^ rest
		  offset: anInteger
		  to: anObject
]

{ #category : 'API' }
ASStackContextPlaceholder >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [

	^ rest
		  offset: anInteger
		  to: anObject
		  context: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'API' }
ASStackContextPlaceholder >> pushPlaceholderContext [ 
self shouldNotImplement 
]

{ #category : 'enumerating' }
ASStackContextPlaceholder >> requiredContextDo: aBlock [

	^ self
]

{ #category : 'initialization' }
ASStackContextPlaceholder >> reset [

	^ self
]
