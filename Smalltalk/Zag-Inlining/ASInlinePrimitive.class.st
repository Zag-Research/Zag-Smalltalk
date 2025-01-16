"
I represent a single method to be inlined that is implemented with a primitive.
That is, I may be embeddable, if the parameters are known to be safe, or embeddable with a fallback.
"
Class {
	#name : 'ASInlinePrimitive',
	#superclass : 'ASInlineSingleton',
	#instVars : [
		'validator'
	],
	#category : 'Zag-Inlining-Inline',
	#package : 'Zag-Inlining',
	#tag : 'Inline'
}

{ #category : 'inlining' }
ASInlinePrimitive >> inline: anASCSend with: anASCodeGenerator [

	^ anASCodeGenerator
		  inlinePrimitive: method
		  for: targetClass
		  from: anASCSend
]

{ #category : 'testing' }
ASInlinePrimitive >> isPrimitive [

	^ true
]

{ #category : 'testing' }
ASInlinePrimitive >> validate: aStack [

	^ (self validator value: aStack) ifTrue: [ self ] ifFalse: [ nil ]
]

{ #category : 'inlining' }
ASInlinePrimitive >> validator [

	^ validator ifNil: [ validator := ASInliner validatorFor: method primitive]
]
