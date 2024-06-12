"
I am a stack element for the `self` position on the stack. This let's code be simpler.
"
Class {
	#name : 'ASStackSelf',
	#superclass : 'ASParameterNamed',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASStackSelf >> findSelf [

	^ self
]

{ #category : 'initialization' }
ASStackSelf >> initialize [

	super initialize.
	name := #self
]

{ #category : 'API' }
ASStackSelf >> intoContext: anInteger [

	isInContext := true.
	offset := anInteger
]
