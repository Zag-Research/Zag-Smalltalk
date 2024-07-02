"
I am a stack element for the `self` position on the stack. This let's code be simpler.
"
Class {
	#name : 'ASStackSelf',
	#superclass : 'ASStackNamed',
	#instVars : [
		'codeBlock'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASStackSelf >> codeBlock: aBlock [
	codeBlock := aBlock
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
	offset := anInteger
]

{ #category : 'data flow instructions' }
ASStackSelf >> makeStackTheSource [

	source := source makeStackTheSource.

]

{ #category : 'enumerating' }
ASStackSelf >> with: other do: aBlock [
	" never go deeper than this "

	^ aBlock value: self value: other
]
