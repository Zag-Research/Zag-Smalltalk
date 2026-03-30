Extension { #name : 'Integer' }

{ #category : '*Zag-Exporter' }
Integer >> bitSize: width rotate: rotation [
	| temp |
	temp := ((self bitShift: width) bitOr: self) bitShift: rotation.
	^ (rotation < 0 ifTrue: [ temp ] ifFalse: [ temp bitShift: width negated ]) bitAnd: (1 bitShift: width) - 1
]
