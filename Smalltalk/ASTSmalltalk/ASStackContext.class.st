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

{ #category : 'enumerating' }
ASStackContext >> do: aBlock [

	^ rest do: aBlock
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

	^ self class new fromStack: (rest newElementFor: aBasicBlock)
]

{ #category : 'accessing' }
ASStackContext >> offset [

	^ 0
]

{ #category : 'initialization' }
ASStackContext >> reset [

	^ self
]
