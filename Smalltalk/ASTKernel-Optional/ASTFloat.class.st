Class {
	#name : #ASTFloat,
	#superclass : #ASTNumber,
	#type : #words,
	#classVars : [
		'E',
		'Epsilon',
		'Halfpi',
		'Infinity',
		'Ln10',
		'Ln2',
		'MaxVal',
		'MaxValLn',
		'MinValLogBase2',
		'NaN',
		'NegativeInfinity',
		'NegativeZero',
		'Pi',
		'RadiansPerDegree',
		'Sqrt2',
		'Twopi'
	],
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
ASTFloat class >> initialize [
	"Float initialize"

	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	Pi := 3.14159265358979323846264338327950288.
	Halfpi := Pi / 2.0.
	Twopi := Pi * 2.0.
	RadiansPerDegree := Pi / 180.0.
	Ln2 := 0.69314718055994530941723212145817657.
	Ln10 := 10.0 ln.
	Sqrt2 := 1.41421356237309504880168872420969808.
	E := 2.718281828459045235360287471353.
	Epsilon := 0.000000000001.	"Defines precision of mathematical functions"
	MaxVal := 1.7976931348623157e308.
	MaxValLn := 709.782712893384.
	MinValLogBase2 := -1074.
	Infinity := MaxVal * MaxVal.
	NegativeInfinity := 0.0 - Infinity.
	NaN := Infinity - Infinity.
	NegativeZero := 1.0 / Infinity negated
]

{ #category : #'as yet unclassified' }
ASTFloat class >> pi [
	"Answer the constant, Pi."

	^ Pi
]

{ #category : #'as yet unclassified' }
ASTFloat class >> readFrom: aStream [
	"Answer a new Float as described on the stream, aStream."

	^ (ASTNumber readFrom: aStream) asFloat
]

{ #category : #'as yet unclassified' }
ASTFloat >> * aNumber [
	"Primitive. Answer the result of multiplying the receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 49>
	^ aNumber adaptToFloat: self andSend: #*
]

{ #category : #'as yet unclassified' }
ASTFloat >> + aNumber [
	"Primitive. Answer the sum of the receiver and aNumber. Essential.
	Fail if the argument is not a Float. See Object documentation
	whatIsAPrimitive."

	<primitive: 41>
	^ aNumber adaptToFloat: self andSend: #+
]

{ #category : #'as yet unclassified' }
ASTFloat >> - aNumber [
	"Primitive. Answer the difference between the receiver and aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 42>
	^ aNumber adaptToFloat: self andSend: #-
]

{ #category : #'as yet unclassified' }
ASTFloat >> / aNumber [
	"Primitive. Answer the result of dividing receiver by aNumber.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 50>
	aNumber = 0
		ifTrue: [ self error: 'attempt to divide by zero' ].
	^ aNumber adaptToFloat: self andSend: #/
]

{ #category : #'as yet unclassified' }
ASTFloat >> < aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 43>
	^ aNumber adaptToFloat: self andSend: #<
]

{ #category : #'as yet unclassified' }
ASTFloat >> <= aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is less than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 45>
	^ aNumber adaptToFloat: self andSend: #<=
]

{ #category : #'as yet unclassified' }
ASTFloat >> = aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 47>
	aNumber isNumber
		ifFalse: [ ^ false ].
	^ aNumber adaptToFloat: self andSend: #=
]

{ #category : #'as yet unclassified' }
ASTFloat >> > aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than the argument. Otherwise return false.
	Fail if the argument is not a Float. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 44>
	^ aNumber adaptToFloat: self andSend: #>
]

{ #category : #'as yet unclassified' }
ASTFloat >> >= aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is greater than or equal to the argument. Otherwise return
	false. Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 46>
	^ aNumber adaptToFloat: self andSend: #>
]

{ #category : #'as yet unclassified' }
ASTFloat >> abs [
	"This is faster than using Number abs."

	self < 0.0
		ifTrue: [ ^ 0.0 - self ]
		ifFalse: [ ^ self ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> absPrintOn: aStream base: base [
	"Print my value on a stream in the given base.  Assumes that my value is strictly
	positive; negative numbers, zero, and NaNs have already been handled elsewhere.
	Based upon the algorithm outlined in:
	Robert G. Burger and R. Kent Dybvig
	Printing Floating Point Numbers Quickly and Accurately
	ACM SIGPLAN 1996 Conference on Programming Language Design and Implementation
	June 1996.
	This version performs all calculations with Floats instead of LargeIntegers, and loses
	about 3 lsbs of accuracy compared to an exact conversion."

	| significantBits fBase exp baseExpEstimate r s mPlus mMinus scale d tc1 tc2 fixedFormat decPointCount |
	self isInfinite
		ifTrue: [ aStream nextPutAll: 'Infinity'.
			^ self ].
	significantBits := 50.	"approximately 3 lsb's of accuracy loss during conversion"
	fBase := base asFloat.
	exp := self exponent.
	baseExpEstimate := (exp * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
	exp >= 0
		ifTrue: [ r := self.
			s := 1.0.
			mPlus := 1.0 timesTwoPower: exp - significantBits.
			mMinus := self significand ~= 1.0
				ifTrue: [ mPlus ]
				ifFalse: [ mPlus / 2.0 ] ]
		ifFalse: [ r := self timesTwoPower: significantBits.
			s := 1.0 timesTwoPower: significantBits.
			mMinus := 1.0 timesTwoPower: (exp max: -1024).
			mPlus := exp = MinValLogBase2 | (self significand ~= 1.0)
				ifTrue: [ mMinus ]
				ifFalse: [ mMinus * 2.0 ] ].
	baseExpEstimate >= 0
		ifTrue: [ s := s * (fBase raisedToInteger: baseExpEstimate).
			exp = 1023
				ifTrue: [ r := r / fBase.
					s := s / fBase.
					mPlus := mPlus / fBase.
					mMinus := mMinus / fBase ]
			"scale down to prevent overflow to Infinity during conversion" ]
		ifFalse: [ exp < -1023
				ifTrue: [ d := (53 * fBase reciprocalLogBase2 - 1.0e-10) ceiling.
					scale := fBase raisedToInteger: d.
					r := r * scale.
					mPlus := mPlus * scale.
					mMinus := mMinus * scale.
					scale := fBase raisedToInteger: (baseExpEstimate + d) negated ]
				ifFalse: [ scale := fBase raisedToInteger: baseExpEstimate negated ].
			"scale up to prevent denorm reciprocals overflowing to Infinity"
			s := s / scale ].
	r + mPlus >= s
		ifTrue: [ baseExpEstimate := baseExpEstimate + 1 ]
		ifFalse: [ s := s / fBase ].
	(fixedFormat := baseExpEstimate between: -3 and: 6)
		ifTrue: [ decPointCount := baseExpEstimate.
			baseExpEstimate <= 0
				ifTrue: [ aStream nextPutAll: ('0.000000' truncateTo: 2 - baseExpEstimate) ] ]
		ifFalse: [ decPointCount := 1 ].
	[ d := (r / s) truncated.
	r := r - (d * s).
	(tc1 := r <= mMinus) | (tc2 := r + mPlus >= s) ]
		whileFalse: [ aStream nextPut: (ASTCharacter digitValue: d).
			r := r * fBase.
			mPlus := mPlus * fBase.
			mMinus := mMinus * fBase.
			decPointCount := decPointCount - 1.
			decPointCount = 0
				ifTrue: [ aStream nextPut: $. ] ].
	tc2
		ifTrue: [ tc1 not | (tc1 & (r * 2.0 >= s))
				ifTrue: [ d := d + 1 ] ].
	aStream nextPut: (ASTCharacter digitValue: d).
	decPointCount > 0
		ifTrue: [ decPointCount - 1 to: 1 by: -1 do: [ :i | aStream nextPut: $0 ].
			aStream nextPutAll: '.0' ].
	fixedFormat
		ifFalse: [ aStream nextPut: $e.
			aStream nextPutAll: (baseExpEstimate - 1) printString ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Float."

	^ rcvr asFloat perform: selector with: self
]

{ #category : #'as yet unclassified' }
ASTFloat >> arcCos [
	"Answer the angle in radians."

	^ Halfpi - self arcSin
]

{ #category : #'as yet unclassified' }
ASTFloat >> arcSin [
	"Answer the angle in radians."

	(self < -1.0 or: [ self > 1.0 ])
		ifTrue: [ self error: 'Value out of range' ].
	(self = -1.0 or: [ self = 1.0 ])
		ifTrue: [ ^ Halfpi ]
		ifFalse: [ ^ (self / (1.0 - (self * self)) sqrt) arcTan ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> arcTan [
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 57>
	| theta eps step sinTheta cosTheta |
	self < 0.0
		ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].	"Newton-Raphson"	"first guess"
	theta := self * Halfpi / (self + 1.0).	"iterate"
	eps := Halfpi * Epsilon.
	step := theta.
	[ step * step > eps ]
		whileTrue: [ sinTheta := theta sin.
			cosTheta := theta cos.
			step := sinTheta * cosTheta - (self * cosTheta * cosTheta).
			theta := theta - step ].
	^ theta
]

{ #category : #'as yet unclassified' }
ASTFloat >> asFloat [
	"Answer the receiver itself."

	^ self
]

{ #category : #'reflective operations' }
ASTFloat >> basicIdentityHash [
	"Answer the floating point high 32-bits xor low 32-bits."
	<primitive: 2171>

]

{ #category : #'as yet unclassified' }
ASTFloat >> cos [
	"Answer the cosine of the receiver taken as an angle in radians."

	^ (self + Halfpi) sin
]

{ #category : #'as yet unclassified' }
ASTFloat >> degreesToRadians [
	"Answer the receiver in radians. Assumes the receiver is in degrees."

	^ self * RadiansPerDegree
]

{ #category : #'as yet unclassified' }
ASTFloat >> exp [
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 59>
	| base fract correction delta div |
	self < 0.0
		ifTrue: [ ^ self negated exp reciprocal ].	"Taylor series"	"check the special cases"
	self = 0.0
		ifTrue: [ ^ 1 ].
	self abs > MaxValLn
		ifTrue: [ self error: 'exp overflow' ].	"get first approximation by raising e to integer power"
	base := E raisedToInteger: self truncated.	"now compute the correction with a short Taylor series"	"fract will be 0..1, so correction will be 1..E"	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract := self fractionPart.
	fract = 0.0
		ifTrue: [ ^ base ].	"no correction required"
	correction := 1.0 + fract.
	delta := fract * fract / 2.0.
	div := 2.0.
	[ delta > Epsilon ]
		whileTrue: [ correction := correction + delta.
			div := div + 1.0.
			delta := delta * fract / div ].
	correction := correction + delta.
	^ base * correction
]

{ #category : #'as yet unclassified' }
ASTFloat >> exponent [
	"Primitive. Consider the receiver to be represented as a power of two
	multiplied by a mantissa (between one and two). Answer with the
	SmallInteger to whose power two is raised. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 53>
	| positive |
	self >= 1.0
		ifTrue: [ ^ self floorLog: 2 ].
	self > 0.0
		ifTrue: [ positive := (1.0 / self) exponent.
			self = (1.0 / (1.0 timesTwoPower: positive))
				ifTrue: [ ^ positive negated ]
				ifFalse: [ ^ positive negated - 1 ] ].
	self = 0.0
		ifTrue: [ ^ -1 ].
	^ self negated exponent
]

{ #category : #'as yet unclassified' }
ASTFloat >> floorLog: radix [
	"Answer the floor of the log base radix of the receiver."

	^ (self log: radix) floor
]

{ #category : #'as yet unclassified' }
ASTFloat >> fractionPart [
	"Primitive. Answer a Float whose value is the difference between the 
	receiver and the receiver's asInteger value. Optional. See Object 
	documentation whatIsAPrimitive."

	<primitive: 52>
	^ self - self truncated asFloat
]

{ #category : #'as yet unclassified' }
ASTFloat >> hash [
	"Hash is reimplemented because = is implemented. Both words of the float are used; 8 bits are removed from each end to clear most of the exponent regardless of the byte ordering. (The bitAnd:'s ensure that the intermediate results do not become a large integer.) Slower than the original version in the ratios 12:5 to 2:1 depending on values. (DNS, 11 May, 1997)"

	^ ((self basicAt: 1) bitAnd: 16r00FFFF00)
		+ ((self basicAt: 2) bitAnd: 16r00FFFF00) bitShift: -8
]

{ #category : #'as yet unclassified' }
ASTFloat >> isInfinite [
	"Return true if the receiver is positive or negative infinity."

	^ self = Infinity or: [ self = NegativeInfinity ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> isNaN [
	"simple, byte-order independent test for Not-a-Number"

	^ self ~= self
]

{ #category : #'as yet unclassified' }
ASTFloat >> ln [
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 58>
	| expt n mant x div pow delta sum eps |
	self <= 0.0
		ifTrue: [ self error: 'ln is only defined for x > 0.0' ].	"Taylor series"	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.	"compute fine correction from mantinssa in Taylor series"	"mant is in the range [0..2]"	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.	"x <= 0"
	eps := Epsilon * (n abs + 1.0).
	[ delta > eps ]
		whileTrue: [ div := div + 1.0.
			pow := pow * x.
			delta := pow / div.
			sum := sum + delta.	"pass two: delta is negative"
			div := div + 1.0.
			pow := pow * x.
			delta := pow / div.
			sum := sum + delta ].
	"pass one: delta is positive"
	^ n + sum	"2.718284 ln 1.0"
]

{ #category : #'as yet unclassified' }
ASTFloat >> log [
	"Answer the base 10 logarithm of the receiver."

	^ self ln / Ln10
]

{ #category : #'as yet unclassified' }
ASTFloat >> negated [
	"Answer a Number that is the negation of the receiver."

	^ 0.0 - self
]

{ #category : #'as yet unclassified' }
ASTFloat >> printOn: aStream base: base [
	"Handle sign, zero, and NaNs; all other values passed to absPrintOn:base:"

	self isNaN
		ifTrue: [ aStream nextPutAll: 'NaN'.
			^ self ].	"check for NaN before sign"
	self > 0.0
		ifTrue: [ self absPrintOn: aStream base: base ]
		ifFalse: [ self sign = -1
				ifTrue: [ aStream nextPutAll: '-' ].
			self = 0.0
				ifTrue: [ aStream nextPutAll: '0.0'.
					^ self ]
				ifFalse: [ self negated absPrintOn: aStream base: base ] ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> radiansToDegrees [
	"Answer the receiver in degrees. Assumes the receiver is in radians."

	^ self / RadiansPerDegree
]

{ #category : #'as yet unclassified' }
ASTFloat >> raisedTo: aNumber [
	"Answer the receiver raised to aNumber."

	0.0 = aNumber
		ifTrue: [ ^ 1.0 ].	"special case for 0.0 raisedTo: 0.0"
	^ (self ln * aNumber asFloat) exp
]

{ #category : #'as yet unclassified' }
ASTFloat >> reciprocal [
	^ 1.0 / self
]

{ #category : #'as yet unclassified' }
ASTFloat >> reciprocalLogBase2 [
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [ Ln2 / Ln10 ]
		ifFalse: [ Ln2 / self ln ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> rounded [
	"Answer the integer nearest the receiver."

	self >= 0.0
		ifTrue: [ ^ (self + 0.5) truncated ]
		ifFalse: [ ^ (self - 0.5) truncated ]
]

{ #category : #'as yet unclassified' }
ASTFloat >> sign [
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0.
	Handle IEEE-754 negative-zero by reporting a sign of -1"

	self > 0
		ifTrue: [ ^ 1 ].
	(self < 0 or: [ ((self at: 1) bitShift: -31) = 1 ])
		ifTrue: [ ^ -1 ].
	^ 0
]

{ #category : #'as yet unclassified' }
ASTFloat >> significand [
	^ self timesTwoPower: self exponent negated
]

{ #category : #'as yet unclassified' }
ASTFloat >> sin [
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 56>
	| sum delta self2 i |
	self < 0.0
		ifTrue: [ ^ 0.0 - (0.0 - self) sin ].	"Taylor series"	"normalize to the range [0..Pi/2]"
	self > Twopi
		ifTrue: [ ^ (self \\ Twopi) sin ].
	self > Pi
		ifTrue: [ ^ 0.0 - (self - Pi) sin ].
	self > Halfpi
		ifTrue: [ ^ (Pi - self) sin ].	"unroll loop to avoid use of abs"
	sum := delta := self.
	self2 := 0.0 - (self * self).
	i := 2.0.
	[ delta > Epsilon ]
		whileTrue: [ delta := delta * self2 / (i * (i + 1.0)).
			i := i + 2.0.
			sum := sum + delta.	"twice"
			delta := delta * self2 / (i * (i + 1.0)).
			i := i + 2.0.
			sum := sum + delta ].
	"once"
	^ sum
]

{ #category : #'as yet unclassified' }
ASTFloat >> sqrt [
	"Answer the square root of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 55>
	| exp guess eps delta |
	self <= 0.0
		ifTrue: [ self = 0.0
				ifTrue: [ ^ 0.0 ]
				ifFalse: [ ^ self error: 'sqrt is invalid for x < 0' ] ].	"Newton-Raphson"	"first guess is half the exponent"
	exp := self exponent // 2.
	guess := self timesTwoPower: 0 - exp.	"get eps value"
	eps := guess * Epsilon.
	eps := eps * eps.
	delta := (self - (guess * guess)) / (guess * 2.0).
	[ delta * delta > eps ]
		whileTrue: [ guess := guess + delta.
			delta := (self - (guess * guess)) / (guess * 2.0) ].
	^ guess
]

{ #category : #'as yet unclassified' }
ASTFloat >> tan [
	"Answer the tangent of the receiver taken as an angle in radians."

	^ self sin / self cos
]

{ #category : #'as yet unclassified' }
ASTFloat >> timesTwoPower: anInteger [
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>
	anInteger < -29
		ifTrue: [ ^ self * (2.0 raisedToInteger: anInteger) ].
	anInteger < 0
		ifTrue: [ ^ self / (1 bitShift: 0 - anInteger) asFloat ].
	anInteger < 30
		ifTrue: [ ^ self * (1 bitShift: anInteger) asFloat ].
	^ self * (2.0 raisedToInteger: anInteger)
]

{ #category : #'as yet unclassified' }
ASTFloat >> truncated [
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>
	(self isInfinite or: [ self isNaN ])
		ifTrue: [ self error: 'Cannot truncate this number' ].
	self abs < 2.0e16
		ifTrue: [ ^ (self quo: 1073741823.0) * 1073741823
				+ (self rem: 1073741823.0) truncated ]
		ifFalse: [ ^ self error: 'not yet implemented' ]
	"Fastest way when it may not be an integer"
]

{ #category : #'as yet unclassified' }
ASTFloat >> ~= aNumber [
	"Primitive. Compare the receiver with the argument and return true
	if the receiver is not equal to the argument. Otherwise return false.
	Fail if the argument is not a Float. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 48>
	^ super ~= aNumber
]
