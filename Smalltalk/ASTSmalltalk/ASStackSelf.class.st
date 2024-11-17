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

{ #category : 'accessing' }
ASStackSelf >> countParameters [

	^ 1
]

{ #category : 'enumerating' }
ASStackSelf >> do: aBlock [
	" never go deeper than this "

	aBlock value: self.

]

{ #category : 'initialization' }
ASStackSelf >> initialize [

	super initialize.
	name := #self
]

{ #category : 'API' }
ASStackSelf >> intoContext: anInteger [

	isInContext := true.
	^ offset := anInteger
]

{ #category : 'data flow instructions' }
ASStackSelf >> makeStackTheSource [

	location := ASFlowParameter from: location
]

{ #category : 'adding' }
ASStackSelf >> nameStack: aCollection offset: anInteger forBlock: aBasicBlock [

	self assert: #self = aCollection first description: 'can''t rename self'
]

{ #category : 'data flow instructions' }
ASStackSelf >> newElementFor: aBasicBlock [

	^ self replicate: next
]

{ #category : 'stack' }
ASStackSelf >> offsetToSelf [

	^ 0
]

{ #category : 'as yet unclassified' }
ASStackSelf >> offsetToSelf: anInteger [

	^ anInteger
]

{ #category : 'enumerating' }
ASStackSelf >> with: other do: aBlock [
	" never go deeper than this "

	^ aBlock value: self value: other
]
