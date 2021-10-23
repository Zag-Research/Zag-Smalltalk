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
	specials = size ifTrue: [ ^ ASBinaryId new value: aString asSymbol ].
	alphas = size ifTrue: [ ^ ASUnaryId new value: aString asSymbol ].
	(last = $: and: [ alphas + 1 = size ]) ifTrue: [ ^ ASKeywordId new value: aString asSymbol ].
	^ ASImproperId new value: aString
]

{ #category : #parsing }
ASIdentifier >> possibleAssign: anASCompiler target: target [ 
	self keywordExpression: anASCompiler target: target
]

{ #category : #parsing }
ASIdentifier >> unaryExpression: anASCompiler target: target [ 
	^ target
]

{ #category : #accessing }
ASIdentifier >> value: aString [
	value := aString asSymbol
]
