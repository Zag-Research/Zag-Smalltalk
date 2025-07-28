"
Strengths are used to measure the relative importance of constraints. The
hierarchy of available strengths is determined by the class variable
StrengthTable (see my class initialization method). Because Strengths are
invariant, references to BeDBStrength instances are shared (i.e. all references to
""BeDBStrength of: #required"" point to a single, shared instance). New strengths may
be inserted in the strength hierarchy without disrupting current constraints.

Instance variables:
	symbolicValue		symbolic strength name (e.g. #required) <Symbol>
	arithmeticValue		index of the constraint in the hierarchy, used for comparisons <Number>

"
Class {
	#name : 'BeDBStrength',
	#superclass : 'Object',
	#instVars : [
		'symbolicValue',
		'arithmeticValue'
	],
	#classVars : [
		'AbsoluteStrongest',
		'AbsoluteWeakest',
		'Required',
		'StrengthConstants',
		'StrengthTable'
	],
	#category : 'Benchmarks-DeltaBlue',
	#package : 'Benchmarks',
	#tag : 'DeltaBlue'
}

{ #category : 'constants' }
BeDBStrength class >> absoluteStrongest [

	^AbsoluteStrongest
]

{ #category : 'constants' }
BeDBStrength class >> absoluteWeakest [

	^AbsoluteWeakest
]

{ #category : 'class initialization' }
BeDBStrength class >> initialize [
	"Initialize the symbolic strength table. Fix the internally caches
	 values of all existing instances."
	"BeDBStrength initialize"

	StrengthTable := Dictionary new.
	StrengthTable at: #absoluteStrongest put: -10000.
	StrengthTable at: #required put: -800.
	StrengthTable at: #strongPreferred put: -600.
	StrengthTable at: #preferred put: -400.
	StrengthTable at: #strongDefault put: -200.
	StrengthTable at: #default put: 0.
	StrengthTable at: #weakDefault put: 500.
	StrengthTable at: #absoluteWeakest put: 10000.

	StrengthConstants := Dictionary new.
	StrengthTable keys do:
		[: strengthSymbol |
			StrengthConstants
				at: strengthSymbol
				put: ((super new) initializeWith: strengthSymbol)].

	AbsoluteStrongest := BeDBStrength of: #absoluteStrongest.
	AbsoluteWeakest := BeDBStrength of: #absoluteWeakest.
	Required := BeDBStrength of: #required.
]

{ #category : 'instance creation' }
BeDBStrength class >> of: aSymbol [
	"Answer an instance with the specified strength."

	^StrengthConstants at: aSymbol
]

{ #category : 'constants' }
BeDBStrength class >> required [

	^Required
]

{ #category : 'private' }
BeDBStrength >> arithmeticValue [
	"Answer my arithmetic value. Used for comparisons. Note that
	 STRONGER constraints have SMALLER arithmetic values."

	^arithmeticValue
]

{ #category : 'private' }
BeDBStrength >> initializeWith: symVal [
	"Record my symbolic value and reset my arithmetic value."

	symbolicValue := symVal.
	arithmeticValue := StrengthTable at: symbolicValue.
]

{ #category : 'printing' }
BeDBStrength >> printOn: aStream [
	"Append a string which represents my strength onto aStream."

	aStream nextPutAll: '%', symbolicValue, '%'.
]

{ #category : 'comparing' }
BeDBStrength >> sameAs: aStrength [
	"Answer true if I am the same strength as the given BeDBStrength."

	^arithmeticValue = aStrength arithmeticValue
]

{ #category : 'comparing' }
BeDBStrength >> stronger: aStrength [
	"Answer true if I am stronger than the given BeDBStrength."

	^arithmeticValue < aStrength arithmeticValue
]

{ #category : 'max/min' }
BeDBStrength >> strongest: aStrength [
	"Answer the stronger of myself and aStrength."

	(aStrength stronger: self)
		ifTrue: [^aStrength]
		ifFalse: [^self].
]

{ #category : 'comparing' }
BeDBStrength >> weaker: aStrength [
	"Answer true if I am weaker than the given BeDBStrength."

	^arithmeticValue > aStrength arithmeticValue
]

{ #category : 'max/min' }
BeDBStrength >> weakest: aStrength [
	"Answer the weaker of myself and aStrength."

	(aStrength weaker: self)
		ifTrue: [^aStrength]
		ifFalse: [^self].
]
