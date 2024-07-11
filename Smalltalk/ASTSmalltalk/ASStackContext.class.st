"
I rempresent a stack context.

I may,  or may not be reified in a particular method
"
Class {
	#name : 'ASStackContext',
	#superclass : 'ASStackNonEmpty',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'displaying' }
ASStackContext >> displayStringOn: stream [

	self printNameOn: stream
]

{ #category : 'enumerating' }
ASStackContext >> do: aBlock [

	^ next do: aBlock
]

{ #category : 'removing' }
ASStackContext >> drop: count [

	count = 0 ifTrue: [ ^ self ].
	^ self error: 'stack underflow'
]

{ #category : 'testing' }
ASStackContext >> hasContext [

	^ true
]

{ #category : 'API' }
ASStackContext >> makeContextRequired: aFullBlockClosure [ 

]

{ #category : 'data flow instructions' }
ASStackContext >> newElementFor: aBasicBlock [
	^ self class new fromStack: (next newElementFor: aBasicBlock)
]

{ #category : 'accessing' }
ASStackContext >> offset [

	^ 0
]

{ #category : 'displaying' }
ASStackContext >> printNameOn: stream [

	stream nextPutAll: '<context>'
]

{ #category : 'initialization' }
ASStackContext >> reset [

	^ self
]

{ #category : 'enumerating' }
ASStackContext >> with: other do: aBlock [

	^ next with: other next do: aBlock
]
