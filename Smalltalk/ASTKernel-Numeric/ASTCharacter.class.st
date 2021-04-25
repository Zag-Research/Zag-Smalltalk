Class {
	#name : #ASTCharacter,
	#superclass : #ASTMagnitude,
	#classVars : [
		'CharacterTable'
	],
	#category : #'ASTKernel-Numeric'
}

{ #category : #'as yet unclassified' }
ASTCharacter class >> asciiValue: anInteger [
	"Answer the Character whose ASCII value is anInteger."

	^ CharacterTable at: anInteger + 1
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> cr [
	"Answer the Character representing a carriage return."

	^ 13 asCharacter
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> digitValue: x [
	"Answer the Character whose digit value is x. For example, answer $9 for x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| i |
	i := x asInteger.
	^ CharacterTable
		at:
			(i < 10
				ifTrue: [ 48 + i ]
				ifFalse: [ 55 + i ]) + 1
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> initialize [
	"Create the table of DigitsValues."

	"self initialize"

	CharacterTable
		ifNil: [ CharacterTable := ASTArray new: 256.
			1 to: 256 do: [ :i | CharacterTable at: i put: (self basicNew setValue: i - 1) ] ]
	"Initialize only once to ensure that byte characters are unique"
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> lf [
	"Answer the Character representing a linefeed."

	^ 10 asCharacter
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> new [
	"There are 256 unique Characters; creating new ones is not allowed."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> setCharacterTable: aCharacterTable [
	CharacterTable := aCharacterTable
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> space [
	"Answer the Character representing a space."

	^ 32 asCharacter
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> tab [
	"Answer the Character representing a tab."

	^ 9 asCharacter
]

{ #category : #'as yet unclassified' }
ASTCharacter class >> value: anInteger [
	"Answer the MCharacter whose ascii value is anInteger."

	^ CharacterTable at: anInteger + 1
]

{ #category : #'as yet unclassified' }
ASTCharacter >> < aCharacter [
	"Answer true if my value is less than the given character's value."

	^ self asciiValue < aCharacter asciiValue
]

{ #category : #'as yet unclassified' }
ASTCharacter >> = aCharacter [
	"Primitive. Answer true if the receiver and the argument are the same object (have the same object pointer) and false otherwise. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 110>
	^ self == aCharacter
]

{ #category : #'as yet unclassified' }
ASTCharacter >> > aCharacter [
	"Answer true if my value is greater than the given character's value."

	^ self asciiValue > aCharacter asciiValue
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asCharacter [
	"Answer the receiver itself."

	^ self
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asInteger [
	"Answer the receiver's character code."
	<primitive: 171>
	^self primitiveFailed
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asLowercase [
	"If I am uppercase, answer the matching lowercase Character. Otherwise, answer myself."

	self isUppercase
		ifTrue: [ ^ (self asInteger + 32) asCharacter ]
		ifFalse: [ ^ self ]	"self isLowercase"
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asString [
	^ ASTString with: self
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asUppercase [

	"If the receiver is lowercase, answer its matching uppercase Character."

	self isLowercase
		ifTrue: [ ^ (self asInteger - 32) asCharacter ]
		ifFalse: [ "self isUppercase" ^ self ]
]

{ #category : #'as yet unclassified' }
ASTCharacter >> asciiValue [
	"Answer the value of the receiver that represents its ascii encoding."

	^ self asInteger
]

{ #category : #'as yet unclassified' }
ASTCharacter >> digitValue [
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and -1 otherwise. This is used to parse literal numbers of radix 2-36."

	self asInteger <= $9 asciiValue
		ifTrue: [ ^ self asInteger - $0 asciiValue ].
	self asInteger >= $A asciiValue
		ifTrue: [ self asInteger <= $Z asciiValue
				ifTrue: [ ^ self asInteger - $A asciiValue + 10 ] ].
	^ -1
]

{ #category : #'as yet unclassified' }
ASTCharacter >> hash [
	"My hash is my value."

	^ self asInteger
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isDigit [
	"Answer whether the receiver is a digit."

	^ self asInteger between: 48 and: 57
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isLetter [

	"Answer whether the receiver is a letter."

	^ self isUppercase or: [ self isLowercase ]
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isLowercase [

	^ self asInteger between: 97 and: 122
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isSpecial [
	"Answer whether the receiver is one of the special characters"

	^ '+/\*~<>=@%|&?!' includes: self
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isUppercase [
	"Answer whether the receiver is an uppercase letter."

	^ self asInteger between: 65 and: 90
]

{ #category : #'as yet unclassified' }
ASTCharacter >> isVowel [
	"Answer true if the receiver is one of the vowels AEIOU (either upper- or lowercase)."

	^ 'AEIOU' includes: self asUppercase
]

{ #category : #printing }
ASTCharacter >> printOn: aStream [
	aStream nextPut: $$.
	aStream nextPut: self
]

{ #category : #'as yet unclassified' }
ASTCharacter >> setValue: newValue [
	^ self error: 'Characters are immutable'
]

{ #category : #copying }
ASTCharacter >> shallowCopy [
	"Answer myself because Characters are unique."

	^ self
]

{ #category : #'as yet unclassified' }
ASTCharacter >> to: other [
	"Answer with a collection of all characters in the given ASCII range. For example, $a to: $z"

	^ (self asciiValue to: other asciiValue)
		collect: [ :i | i asCharacter ]
]

{ #category : #'as yet unclassified' }
ASTCharacter >> tokenish [
	"Answer true if the receiver is a valid token-character--that is, a letter, digit, or colon."

	^ self isLetter or: [ self isDigit or: [ self = $: ] ]
]
