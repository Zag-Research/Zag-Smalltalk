Class {
	#name : #PCLargePositiveInteger,
	#superclass : #PCInteger,
	#type : #bytes,
	#category : #'ASTKernel-Numeric'
}

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> * anInteger [
	"Primitive. Multiply the receiver by the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 29>
	^ super * anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> + anInteger [
	"Primitive. Add the receiver to the argument and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 21>
	^ super + anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> - anInteger [
	"Primitive. Subtract the argument from the receiver and answer with an
	Integer result. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 22>
	^ super - anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> / anInteger [
	"Primitive. Divide the receiver by the argument and answer with the
	result if the division is exact. Fail if the result is not a whole integer.
	Fail if the argument is 0. Fail if either the argument or the result is not
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive. "

	<primitive: 30>
	^ super / anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> // anInteger [
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards negative infinity to make it a whole
	integer. Fail if the argument is 0. Fail if either the argument or the
	result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive. "

	<primitive: 32>
	^ super // anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> < anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than the argument. Otherwise answer false. Fail if the
	argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824).
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 23>
	^ super < anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> <= anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is less than or equal to the argument. Otherwise answer false.
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 25>
	^ super <= anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> = anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	receiver or argument is negative or greater than 32 bits.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 7>
	^ super = anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> > anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than the argument. Otherwise answer false. Fail if
	the argument is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 24>
	^ super > anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> >= anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is greater than or equal to the argument. Otherwise answer
	false. Fail if the argument is not a SmallInteger or a LargePositiveInteger
	less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 26>
	^ super >= anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> \\ anInteger [
	"Primitive. Take the receiver modulo the argument. The result is the
	remainder rounded towards negative infinity, of the receiver divided
	by the argument. Fail if the argument is 0. Fail if either the argument
	or the result is not a SmallInteger or a LargePositiveInteger less than
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive."

	<primitive: 31>
	^ super \\ anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> abs [
	
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> bitAnd: anInteger [
	"Primitive. Answer an Integer whose bits are the logical AND of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."

	<primitive: 14>
	^ super bitAnd: anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> bitOr: anInteger [
	"Primitive. Answer an Integer whose bits are the logical OR of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."

	<primitive: 15>
	^ super bitOr: anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> bitShift: anInteger [
	"Primitive. Answer an Integer whose value (in twos-complement 
	representation) is the receiver's value (in twos-complement
	representation) shifted left by the number of bits indicated by the
	argument. Negative arguments shift right. Zeros are shifted in from the
	right in left shifts. The sign bit is extended in right shifts.
	Fail if the receiver or result is greater than 32 bits.
	See Object documentation whatIsAPrimitive."

	<primitive: 17>
	^ super bitShift: anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> bitXor: anInteger [
	"Primitive. Answer an Integer whose bits are the logical XOR of the
	receiver's bits and those of the argument. Fail if the receiver or argument
	is greater than 32 bits. See Object documentation whatIsAPrimitive."

	<primitive: 16>
	^ super bitXor: anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> digitAt: index [
	"Primitive. Answer the value of an indexable field in the receiver. Fail if 
	the argument (the index) is not an Integer or is out of bounds. Essential. 
	See Object documentation whatIsAPrimitive."

	<primitive: 60>
	self digitLength < index
		ifTrue: [ ^ 0 ]
		ifFalse: [ ^ super at: index ]
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> digitAt: index put: value [
	"Primitive. Store the second argument (value) in the indexable field of 
	the receiver indicated by index. Fail if the value is negative or is larger 
	than 255. Fail if the index is not an Integer or is out of bounds. Answer 
	the value that was stored. Essential. See Object documentation 
	whatIsAPrimitive."

	<primitive: 61>
	^ super at: index put: value
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> digitLength [
	"Primitive. Answer the number of indexable fields in the receiver. This 
	value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive."

	<primitive: 62>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> highBit [
	"Answer the index of the high order bit of the receiver, or zero if the receiver is zero. This method is allowed (and needed) for LargeNegativeIntegers as well, since LargeIntegers are sign/magnitude."

	| realLength lastDigit |
	realLength := self digitLength.
	[ (lastDigit := self digitAt: realLength) = 0 ]
		whileTrue: [ (realLength := realLength - 1) = 0
				ifTrue: [ ^ 0 ] ].
	^ lastDigit highBit + (8 * (realLength - 1))
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> negated [
	^ (self copyto: (PCLargeNegativeInteger new: self digitLength))
		normalize	"Need to normalize to catch SmallInteger minVal"
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> negative [
	"Answer whether the receiver is mathematically negative."

	^ false
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> normalize [
	"Check for leading zeroes and return shortened copy if so"

	| sLen val len oldLen |
	"First establish len = significant length"
	len := oldLen := self digitLength.
	[ len = 0
		ifTrue: [ ^ 0 ].
	(self digitAt: len) = 0 ] whileTrue: [ len := len - 1 ].	"Now check if in SmallInteger range"
	sLen := PCSmallInteger maxVal digitLength.
	(len <= sLen
		and: [ (self digitAt: sLen) <= (PCSmallInteger maxVal digitAt: sLen) ])
		ifTrue: [ val := 0.
			len to: 1 by: -1 do: [ :i | val := val * 256 + (self digitAt: i) ].
			^ val ].
	"If so, return its SmallInt value"	"Return self, or a shortened copy"
	len < oldLen
		ifTrue: [ ^ self growto: len ]
		ifFalse: [ ^ self ]
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> quo: anInteger [
	"Primitive. Divide the receiver by the argument and return the result.
	Round the result down towards zero to make it a whole integer. Fail if
	the argument is 0. Fail if either the argument or the result is not a
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See
	Object documentation whatIsAPrimitive."

	<primitive: 33>
	^ super quo: anInteger
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105>
	^ super
		replaceFrom: start
		to: stop
		with: replacement
		startingAt: repStart
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> sign [
	"Optimization. Answer 1 since receiver is greater than 0."

	^ 1
]

{ #category : #'as yet unclassified' }
PCLargePositiveInteger >> ~= anInteger [
	"Primitive. Compare the receiver with the argument and answer true if
	the receiver is equal to the argument. Otherwise answer false. Fail if the
	receiver or argument is negative or greater than 32 bits.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 8>
	^ super ~= anInteger
]
