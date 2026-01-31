Class {
	#name : 'ASIdentifierToken',
	#superclass : 'ASSimple',
	#classInstVars : [
		'classMap'
	],
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'class initialization' }
ASIdentifierToken class >> initialize [

	classMap := {
		            (#self -> ASSelfToken).
		            (#super -> ASSelfToken).
		            (#true -> ASSpecial).
		            (#false -> ASSpecial).
		            (#nil -> ASSpecial).
		            (#thisContext -> ASSpecial)
			} asDictionary
]

{ #category : 'scanning' }
ASIdentifierToken class >> with: aString [

	| specials alphas colons last size |
	classMap at: aString ifPresent: [ : class | ^ class new value: aString].
	specials := alphas := colons := size := 0.
	last := $..
	aString do: [ :c |
			c isSpecial ifTrue: [ specials := specials + 1 ].
			(c isLetter or: [ c = $_ or: [ c isDigit and: [ alphas > 0 ] ] ])
				ifTrue: [ alphas := alphas + 1 ].
			last := c.
			size := size + 1 ].
	specials = size ifTrue: [ ^ ASBinaryId new value: aString asSymbol ].
	alphas = size ifTrue: [ ^ ASUnaryId new value: aString asSymbol ].
	(last = $: and: [ alphas + 1 = size ]) ifTrue: [
		^ ASKeywordId new value: aString asSymbol ].
	^ ASImproperId new value: aString
]

{ #category : 'parsing' }
ASIdentifierToken >> possibleAssign: anASCompiler target: target [ 
	self keywordExpression: anASCompiler target: target
]

{ #category : 'parsing' }
ASIdentifierToken >> unaryExpression: anASCompiler target: target [ 
	^ target
]

{ #category : 'accessing' }
ASIdentifierToken >> value: aString [
	value := aString asSymbol
]
