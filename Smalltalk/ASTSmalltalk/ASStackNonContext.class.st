"
All the things on the stack with real content
"
Class {
	#name : 'ASStackNonContext',
	#superclass : 'ASStackNonEmpty',
	#instVars : [
		'source'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'evaluating' }
ASStackNonContext >> valueClass [

	^ source valueClass
]
