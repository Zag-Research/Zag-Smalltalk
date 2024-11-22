"
Placeholder for an `ASContext`

The size is the offset between the last named element and the first temporary
"
Class {
	#name : 'ASStackContext',
	#superclass : 'ASStack',
	#instVars : [
		'size',
		'next',
		'extra',
		'parameters'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'printing' }
ASStackContext class >> canonicalArgumentName [

	^ 'aStackContext'
]

{ #category : 'flow analysis' }
ASStackContext >> addFlow: aBasicBlock [

	^ next addFlow: aBasicBlock
]

{ #category : 'enumerating' }
ASStackContext >> addToCollection: aCollection [

	aCollection addLast: self.
	^ next addToCollection: aCollection
]

{ #category : 'accessing' }
ASStackContext >> at: aSymbol [

	^ next at: aSymbol
]

{ #category : 'accessing' }
ASStackContext >> context [

	^ self
]

{ #category : 'displaying' }
ASStackContext >> displayStringOn: stream [

	self printNameOn: stream
]

{ #category : 'accessing' }
ASStackContext >> extra [

	^ extra
]

{ #category : 'instance creation' }
ASStackContext >> fromStack: anASStack [

	next := anASStack
]

{ #category : 'accessing' }
ASStackContext >> growBy: anInteger [

self halt.
	size := 1 + size.
	extra := extra + anInteger
]

{ #category : 'initialization' }
ASStackContext >> initialize [

	super initialize.
	size := parameters := extra := 0
]

{ #category : 'accessing' }
ASStackContext >> locals [

	^ size - parameters
]

{ #category : 'accessing' }
ASStackContext >> offset [

	^ next offset
]

{ #category : 'displaying' }
ASStackContext >> printNameOn: stream [

	^ stream nextPutAll: '<context>'
]

{ #category : 'accessing' }
ASStackContext >> required [

	size = 0 ifTrue: [
		size := next intoContext: 1.
		parameters := next countParameters ]
]

{ #category : 'accessing' }
ASStackContext >> size [

	^ size
]
