Class {
	#name : #ASIdentifier,
	#superclass : #ASSimple,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #scanning }
ASIdentifier class >> with: aString [
	| specials alphas colons last size |
	specials := alphas := colons := size :=0.
	last := $..
	aString do: [ : c |
		c isSpecial ifTrue: [ specials := specials + 1 ].
		(c isLetter or: [ c=$_ or: [c isDigit and: [alphas > 0]]]) ifTrue: [ alphas := alphas + 1 ].
		last := c.
		size := size + 1
	].
	specials = size ifTrue: [ ^ ASBinaryId new value: aString ].
	alphas = size ifTrue: [ ^ ASUnaryId new value: aString ].
	(last = $: and: [ alphas + 1 = size ]) ifTrue: [ ^ ASKeywordId new value: aString ].
	^ self new value: aString
]

{ #category : #accessing }
ASIdentifier >> arity [
	| last |
	last := value last.
	last tokenish ifFalse: [ ^ 2 ].
	(value includes: $:) ifFalse: [ ^ 1 ].
	last = $: ifFalse: [ ^ 0 ].
	^ 1+(value count: [: c | c=$:])
]

{ #category : #parsing }
ASIdentifier >> processKeyword: anASCompiler target: target [ 
	self isUnary ifTrue: [ ^ ASSend new target: target selector: self args: #() ].
	self halt
]

{ #category : #parsing }
ASIdentifier >> processUnary: anASCompiler target: target [
	self isUnary ifFalse: [ ^ target ].
	^ self processUnary: anASCompiler target: (ASSend target: target selector: self args: #())
]
