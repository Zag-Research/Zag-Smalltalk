Class {
	#name : 'ASString',
	#superclass : 'ASLiteralToken',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
	#tag : 'Tokens'
}

{ #category : 'accessing' }
ASString >> arity [
	| last |
	last := value last.
	last tokenish ifFalse: [ ^ 2 ].
	(value includes: $:) ifFalse: [ ^ 1 ].
	last = $: ifFalse: [ ^ 0 ].
	^ 1+(value count: [: c | c=$:])
]
