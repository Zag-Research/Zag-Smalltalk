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

{ #category : 'removing' }
ASStackContext >> drop: count [

	count = 0 ifTrue: [ ^ self ].
	^ self error: 'stack underflow'
]

{ #category : 'initialization' }
ASStackContext >> reset [

	^ self
]
