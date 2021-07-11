Class {
	#name : #ASTInteger,
	#superclass : #ASTNumber,
	#category : #'ASTKernel-Numeric'
}

{ #category : #'as yet unclassified' }
ASTInteger class >> new: length neg: neg [
	"Answer an instance of a large integer with the given size and sign."

	neg
		ifTrue: [ ^ ASTLargeNegativeInteger new: length ]
		ifFalse: [ ^ ASTLargePositiveInteger new: length ]
]

{ #category : #'as yet unclassified' }
ASTInteger class >> readFrom: aStream [
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."

	^ self readFrom: aStream base: 10
]

{ #category : #'as yet unclassified' }
ASTInteger class >> readFrom: aStream base: base [
	"Answer an instance of one of my concrete subclasses. Initial minus sign 
	accepted, and bases > 10 use letters A-Z. Embedded radix specifiers not 
	allowed--use Number readFrom: for that. Answer zero (not an error) if 
	there are no digits."

	| digit value neg |
	neg := aStream peekFor: $-.
	value := 0.
	[ aStream atEnd ]
		whileFalse: [ digit := aStream next digitValue.
			(digit < 0 or: [ digit >= base ])
				ifTrue: [ aStream skip: -1.
					neg
						ifTrue: [ ^ value negated ].
					^ value ]
				ifFalse: [ value := value * base + digit ] ].
	neg
		ifTrue: [ ^ value negated ].
	^ value
]

{ #category : #'as yet unclassified' }
ASTInteger >> * aNumber [
	"Refer to the comment in Number * "

	aNumber isInteger
		ifTrue:
			[ ^ self digitMultiply: aNumber neg: self negative ~~ aNumber negative ].
	^ aNumber adaptToInteger: self andSend: #*
]

{ #category : #'as yet unclassified' }
ASTInteger >> + aNumber [
	"Refer to the comment in Number + "

	aNumber isInteger
		ifTrue: [ self negative == aNumber negative
				ifTrue: [ ^ (self digitAdd: aNumber) normalize ]
				ifFalse: [ ^ self digitSubtract: aNumber ] ].
	^ aNumber adaptToInteger: self andSend: #+
]

{ #category : #'as yet unclassified' }
ASTInteger >> - aNumber [
	"Refer to the comment in Number - "

	aNumber isInteger
		ifTrue: [ self negative == aNumber negative
				ifTrue: [ ^ self digitSubtract: aNumber ]
				ifFalse: [ ^ (self digitAdd: aNumber) normalize ] ].
	^ aNumber adaptToInteger: self andSend: #-
]

{ #category : #'as yet unclassified' }
ASTInteger >> / aNumber [
	"Refer to the comment in Number / "

	| quoRem |
	aNumber isInteger
		ifTrue: [ quoRem := self
				digitDiv: aNumber abs
				neg: self negative ~~ aNumber negative.
			(quoRem at: 2) = 0
				ifTrue: [ ^ (quoRem at: 1) normalize ]
				ifFalse: [ ^ self asFloat / aNumber asFloat ] ].
	^ aNumber adaptToInteger: self andSend: #/
]

{ #category : #'as yet unclassified' }
ASTInteger >> // aNumber [
	| q |
	aNumber = 0
		ifTrue: [ ^ self error: 'division by 0' ].
	self = 0
		ifTrue: [ ^ 0 ].
	q := self quo: aNumber.	"Refer to the comment in Number|//."
	(q negative
		ifTrue: [ q * aNumber ~= self ]
		ifFalse: [ q = 0 and: [ self negative ~= aNumber negative ] ])
		ifTrue: [ ^ q - 1	"Truncate towards minus infinity" ]
		ifFalse: [ ^ q ]
]

{ #category : #'as yet unclassified' }
ASTInteger >> < aNumber [
	aNumber isInteger
		ifTrue: [ self negative == aNumber negative
				ifTrue: [ self negative
						ifTrue: [ ^ (self digitCompare: aNumber) > 0 ]
						ifFalse: [ ^ (self digitCompare: aNumber) < 0 ] ]
				ifFalse: [ ^ self negative ] ].
	^ aNumber adaptToInteger: self andSend: #<
]

{ #category : #'as yet unclassified' }
ASTInteger >> = aNumber [
	aNumber isNumber
		ifFalse: [ ^ false ].
	aNumber isInteger
		ifTrue: [ aNumber negative == self negative
				ifTrue: [ ^ (self digitCompare: aNumber) = 0 ]
				ifFalse: [ ^ false ] ].
	^ aNumber adaptToInteger: self andSend: #=
]

{ #category : #'as yet unclassified' }
ASTInteger >> > aNumber [
	aNumber isInteger
		ifTrue: [ self negative == aNumber negative
				ifTrue: [ self negative
						ifTrue: [ ^ (self digitCompare: aNumber) < 0 ]
						ifFalse: [ ^ (self digitCompare: aNumber) > 0 ] ]
				ifFalse: [ ^ aNumber negative ] ].
	^ aNumber adaptToInteger: self andSend: #>
]

{ #category : #'as yet unclassified' }
ASTInteger >> asCharacter [
	"Answer the Character whose value is the receiver."

	^ ASTCharacter asciiValue: self
]

{ #category : #'as yet unclassified' }
ASTInteger >> asFloat [
	"Answer a Float that represents the value of the receiver.
	Optimized to process only the significant digits of a LargeInteger."

	| sum firstByte shift |
	shift := 0.
	sum := 0.
	firstByte := self size - 7 max: 1.
	firstByte to: self size do: [ :byteIndex | 
		sum := ((self digitAt: byteIndex) asFloat timesTwoPower: shift) + sum.
		shift := shift + 8 ].
	^ sum * self sign asFloat timesTwoPower: (firstByte - 1) * 8
]

{ #category : #'as yet unclassified' }
ASTInteger >> asInteger [
	"Answer with the receiver itself."

	^ self
]

{ #category : #benchmarks }
ASTInteger >> benchFib [
	"Handy send-heavy benchmark"

	"(result // seconds to run) = approx calls per second"

	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"

	"138000 on a Mac 8100/100"

	^ self < 2
		ifTrue: [ 1 ]
		ifFalse: [ (self - 1) benchFib + (self - 2) benchFib + 1 ]
]

{ #category : #benchmarks }
ASTInteger >> benchmark [  "Handy bytecode-heavy benchmark"
	"(500000 // time to run) = approx bytecodes per second"
	"5000000 // (Time millisecondsToRun: [10 benchmark]) * 1000"
	"3059000 on a Mac 8100/100"
	| size flags prime k count |
	size := 8190.
	1 to: self do: [ :iter | 
		count := 0.
		flags := (1 to: size) collect: [ :i | true ].
		1 to: size do: [ :i | 
			(flags at: i)
				ifTrue: [ prime := i + 1.
					k := i + prime.
					[ k <= size ]
						whileTrue: [ flags at: k put: false.
							k := k + prime ].
					count := count + 1 ] ] ].
	^ count
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitAnd: n [
	"Answer an Integer whose bits are the logical AND of the receiver's bits 
	and those of the argument, n."

	| norm |
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitAnd:
		length: (self digitLength max: norm digitLength)
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitClear: aMask [
	"Answer an Integer equal to the receiver, except with all bits cleared that are set in aMask."

	^ (self bitOr: aMask) - aMask
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitInvert [
	"Answer an Integer whose bits are the logical negation of the receiver's bits.
	Numbers are interpreted as having 2's-complement representation."

	^ -1 - self
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitOr: n [
	"Answer an Integer whose bits are the logical OR of the receiver's bits 
	and those of the argument, n."

	| norm |
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitOr:
		length: (self digitLength max: norm digitLength)
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitShift: shiftCount [
	"Answer an Integer whose value (in twos-complement representation) is 
	the receiver's value (in twos-complement representation) shifted left by 
	the number of bits indicated by the argument. Negative arguments shift 
	right. Zeros are shifted in from the right in left shifts."

	| rShift |
	shiftCount >= 0
		ifTrue: [ ^ self digitLshift: shiftCount ].
	rShift := 0 - shiftCount.
	^ (self
		digitRshift: (rShift bitAnd: 7)
		bytes: (rShift bitShift: -3)
		lookfirst: self digitLength) normalize
]

{ #category : #'as yet unclassified' }
ASTInteger >> bitXor: n [
	"Answer an Integer whose bits are the logical XOR of the receiver's bits 
	and those of the argument, n."

	| norm |
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitXor:
		length: (self digitLength max: norm digitLength)
]

{ #category : #'as yet unclassified' }
ASTInteger >> ceiling [
	"Refer to the comment in Number|ceiling."

	
]

{ #category : #'as yet unclassified' }
ASTInteger >> copyto: x [
	| stop |
	stop := self digitLength min: x digitLength.
	^ x
		replaceFrom: 1
		to: stop
		with: self
		startingAt: 1
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitAdd: arg [
	| len arglen accum sum |
	accum := 0.
	(len := self digitLength) < (arglen := arg digitLength)
		ifTrue: [ len := arglen ].	"Open code max: for speed"
	sum := ASTInteger new: len neg: self negative.
	1 to: len do: [ :i | 
		accum := (accum bitShift: -8) + (self digitAt: i) + (arg digitAt: i).
		sum digitAt: i put: (accum bitAnd: 255) ].
	accum > 255
		ifTrue: [ sum := sum growby: 1.
			sum at: sum digitLength put: (accum bitShift: -8) ].
	^ sum
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitCompare: arg [
	"Compare the magnitude of self with that of arg.
	Return a code of 1, 0, -1 for self >, = , < arg"

	| len arglen argDigit selfDigit |
	len := self digitLength.
	(arglen := arg digitLength) ~= len
		ifTrue: [ arglen > len
				ifTrue: [ ^ -1 ]
				ifFalse: [ ^ 1 ] ].
	[ len > 0 ]
		whileTrue: [ (argDigit := arg digitAt: len) ~= (selfDigit := self digitAt: len)
				ifTrue: [ argDigit < selfDigit
						ifTrue: [ ^ 1 ]
						ifFalse: [ ^ -1 ] ].
			len := len - 1 ].
	^ 0
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitDiv: arg neg: ng [
	"Answer with an array of (quotient, remainder)."

	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t |
	arg = 0
		ifTrue: [ ^ self error: 'division by 0' ].
	l := self digitLength - arg digitLength + 1.
	l <= 0
		ifTrue: [ ^ ASTArray with: 0 with: self ].
	d := 8 - arg lastDigit highBit.
	div := arg digitLshift: d.
	div := div growto: div digitLength + 1.	"shifts so high order word is >=128"
	rem := self digitLshift: d.
	rem digitLength = self digitLength
		ifTrue: [ rem := rem growto: self digitLength + 1 ].	"makes a copy and shifts"
	quo := ASTInteger new: l neg: ng.
	dl := div digitLength - 1.	"Last actual byte of data"
	ql := l.
	dh := div digitAt: dl.
	dnh := dl = 1
		ifTrue: [ 0 ]
		ifFalse: [ div digitAt: dl - 1 ].
	1 to: ql do: [ :k | 
		j := rem digitLength + 1 - k.	"r1 := rem digitAt: j."
		(rem digitAt: j) = dh
			ifTrue: [ qhi := qlo := 15	"i.e. q=255" ]
			ifFalse: [ t := ((rem digitAt: j) bitShift: 4)
					+ ((rem digitAt: j - 1) bitShift: -4).
				qhi := t // dh.
				t := (t \\ dh bitShift: 4) + ((rem digitAt: j - 1) bitAnd: 15).
				qlo := t // dh.
				t := t \\ dh.	"Next compute (hi,lo) := q*dnh"
				hi := qhi * dnh.
				lo := qlo * dnh + ((hi bitAnd: 15) bitShift: 4).
				hi := (hi bitShift: -4) + (lo bitShift: -8).
				lo := lo bitAnd: 255.	"Correct overestimate of q.  
					Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3
					ifTrue: [ 0 ]
					ifFalse: [ rem digitAt: j - 2 ].
				[ (t < hi or: [ t = hi and: [ r3 < lo ] ])
					and: [ qlo := qlo - 1.
						lo := lo - dnh.
						lo < 0
							ifTrue: [ hi := hi - 1.
								lo := lo + 256 ].
						hi >= dh ]
				"i.e. (t,r3) < (hi,lo)" ] whileTrue: [ hi := hi - dh ].
				qlo < 0
					ifTrue: [ qhi := qhi - 1.
						qlo := qlo + 16 ] ].
		"Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.  
					Note that r1,r2 are bytes, not nibbles.  
					Be careful not to generate intermediate results exceeding 13 bits."
		"r2 := (rem digitAt: j - 1)."	"Subtract q*div from rem"
		l := j - dl.
		a := 0.
		1 to: div digitLength do: [ :i | 
			hi := (div digitAt: i) * qhi.
			lo := a + (rem digitAt: l) - ((hi bitAnd: 15) bitShift: 4)
				- ((div digitAt: i) * qlo).
			rem digitAt: l put: lo - (lo // 256 * 256).	"sign-tolerant form of (lo bitAnd: 255)"
			a := lo // 256 - (hi bitShift: -4).
			l := l + 1 ].
		a < 0
			ifTrue: [ qlo := qlo - 1.
				l := j - dl.
				a := 0.
				1 to: div digitLength do: [ :i | 
					a := (a bitShift: -8) + (rem digitAt: l) + (div digitAt: i).
					rem digitAt: l put: (a bitAnd: 255).
					l := l + 1 ] ].
		"Add div back into rem, decrease q by 1"
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4) + qlo ].
	"maintain quo*arg+rem=self"
	"Estimate rem/div by dividing the leading to bytes of rem by dh."
	"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles."
	rem := rem digitRshift: d bytes: 0 lookfirst: dl.
	^ ASTArray with: quo with: rem
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitLogic: arg op: op length: len [
	| result neg1 neg2 rneg z1 z2 rz b1 b2 b |
	neg1 := self negative.
	neg2 := arg negative.
	rneg := ((neg1
		ifTrue: [ -1 ]
		ifFalse: [ 0 ])
		perform: op
		with:
			(neg2
				ifTrue: [ -1 ]
				ifFalse: [ 0 ])) < 0.
	result := ASTInteger new: len neg: rneg.
	rz := z1 := z2 := true.
	1 to: result digitLength do: [ :i | 
		b1 := self digitAt: i.
		neg1
			ifTrue: [ b1 := z1
					ifTrue: [ b1 = 0
							ifTrue: [ 0 ]
							ifFalse: [ z1 := false.
								256 - b1 ] ]
					ifFalse: [ 255 - b1 ] ].
		b2 := arg digitAt: i.
		neg2
			ifTrue: [ b2 := z2
					ifTrue: [ b2 = 0
							ifTrue: [ 0 ]
							ifFalse: [ z2 := false.
								256 - b2 ] ]
					ifFalse: [ 255 - b2 ] ].
		b := b1 perform: op with: b2.
		b = 0
			ifTrue: [ result digitAt: i put: 0 ]
			ifFalse: [ result
					digitAt: i
					put:
						(rneg
							ifTrue: [ rz
									ifTrue: [ rz := false.
										256 - b ]
									ifFalse: [ 255 - b ] ]
							ifFalse: [ b ]) ] ].
	^ result normalize
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitLshift: shiftCount [
	| carry rShift mask len result digit byteShift bitShift highBit |
	(highBit := self highBit) = 0
		ifTrue: [ ^ 0 ].
	len := (highBit + shiftCount + 7) // 8.
	result := ASTInteger new: len neg: self negative.
	byteShift := shiftCount // 8.
	bitShift := shiftCount \\ 8.
	bitShift = 0
		ifTrue: [ ^ result
				replaceFrom: byteShift + 1
				to: len
				with: self
				startingAt: 1 ].
	"Fast version for byte-aligned shifts"
	carry := 0.
	rShift := bitShift - 8.
	mask := 255 bitShift: 0 - bitShift.
	1 to: byteShift do: [ :i | result digitAt: i put: 0 ].
	1 to: len - byteShift do: [ :i | 
		digit := self digitAt: i.
		result
			digitAt: i + byteShift
			put: (((digit bitAnd: mask) bitShift: bitShift) bitOr: carry).
		carry := digit bitShift: rShift ].
	^ result
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitMultiply: arg neg: ng [
	| prod prodLen carry digit k ab |
	(arg digitLength = 1 and: [ (arg digitAt: 1) = 0 ])
		ifTrue: [ ^ 0 ].
	prodLen := self digitLength + arg digitLength.
	prod := ASTInteger new: prodLen neg: ng.	"prod starts out all zero"
	1 to: self digitLength do: [ :i | 
		(digit := self digitAt: i) ~= 0
			ifTrue: [ k := i.
				carry := 0.	"Loop invariant: 0<=carry<=0377, k=i+j-1"
				1 to: arg digitLength do: [ :j | 
					ab := (arg digitAt: j) * digit + carry + (prod digitAt: k).
					carry := ab bitShift: -8.
					prod digitAt: k put: (ab bitAnd: 255).
					k := k + 1 ].
				prod digitAt: k put: carry ] ].
	^ prod normalize
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitRshift: anInteger bytes: b lookfirst: a [
	"Shift right 8*b+anInteger bits, 0<=n<8.
	Discard all digits beyond a, and all zeroes at or below a."

	| n x r f m digit count i |
	n := 0 - anInteger.
	x := 0.
	f := n + 8.
	i := a.
	m := 255 bitShift: 0 - f.
	digit := self digitAt: i.
	[ ((digit bitShift: n) bitOr: x) = 0 and: [ i ~= 1 ] ]
		whileTrue: [ x := digit bitShift: f.	"Can't exceed 8 bits"
			i := i - 1.
			digit := self digitAt: i ].
	i <= b
		ifTrue: [ ^ ASTInteger new: 0 neg: self negative ].	"All bits lost"
	r := ASTInteger new: i - b neg: self negative.
	count := i.
	x := (self digitAt: b + 1) bitShift: n.
	b + 1 to: count do: [ :j | 
		digit := self digitAt: j + 1.
		r digitAt: j - b put: (((digit bitAnd: m) bitShift: f) bitOr: x).	"Avoid values > 8 bits"
		x := digit bitShift: n ].
	^ r
]

{ #category : #'as yet unclassified' }
ASTInteger >> digitSubtract: arg [
	| smaller larger z sum sl al ng |
	sl := self digitLength.
	al := arg digitLength.
	(sl = al
		ifTrue: [ [ (self digitAt: sl) = (arg digitAt: sl) and: [ sl > 1 ] ]
				whileTrue: [ sl := sl - 1 ].
			al := sl.
			(self digitAt: sl) < (arg digitAt: sl) ]
		ifFalse: [ sl < al ])
		ifTrue: [ larger := arg.
			smaller := self.
			ng := self negative == false.
			sl := al ]
		ifFalse: [ larger := self.
			smaller := arg.
			ng := self negative ].
	sum := ASTInteger new: sl neg: ng.
	z := 0.	"Loop invariant is -1<=z<=1"
	1 to: sl do: [ :i | 
		z := z + (larger digitAt: i) - (smaller digitAt: i).
		sum digitAt: i put: z - (z // 256 * 256).	"sign-tolerant form of (z bitAnd: 255)"
		z := z // 256 ].
	^ sum normalize
]

{ #category : #'as yet unclassified' }
ASTInteger >> floor [
	"Refer to the comment in Number|floor."

	
]

{ #category : #'as yet unclassified' }
ASTInteger >> growby: n [
	^ self growto: self digitLength + n
]

{ #category : #'as yet unclassified' }
ASTInteger >> growto: n [
	^ self copyto: (self species new: n)
]

{ #category : #'as yet unclassified' }
ASTInteger >> hash [
	"Hash is reimplemented because = is implemented."

	^ (self lastDigit bitShift: 8) + (self digitAt: 1)
]

{ #category : #'as yet unclassified' }
ASTInteger >> isInteger [
	"True for all subclasses of Integer."

	^ true
]

{ #category : #'as yet unclassified' }
ASTInteger >> lastDigit [
	"Answer the last digit of the integer."

	^ self digitAt: self digitLength
]

{ #category : #'as yet unclassified' }
ASTInteger >> normalize [
	"SmallInts OK; LgInts override"

	^ self
]

{ #category : #'as yet unclassified' }
ASTInteger >> printOn: aStream base: b [
	"Print a representation of the receiver on the stream, aStream, in base, b, 
	where 2<=b<=16."

	| digits source dest i j pos t rem |
	b = 10
		ifFalse: [ b printOn: aStream.
			aStream nextPut: $r ].
	i := self digitLength.	"Estimate size of result, conservatively"
	digits := ASTArray new: i * 8.
	pos := 0.
	dest := i <= 1
		ifTrue: [ self ]
		ifFalse: [ ASTLargePositiveInteger new: i ].
	source := self.
	[ i >= 1 ]
		whileTrue: [ rem := 0.
			j := i.
			[ j > 0 ]
				whileTrue: [ t := (rem bitShift: 8) + (source digitAt: j).
					dest digitAt: j put: t // b.
					rem := t \\ b.
					j := j - 1 ].
			pos := pos + 1.
			digits at: pos put: rem.
			source := dest.
			(source digitAt: i) = 0
				ifTrue: [ i := i - 1 ] ].	"(dest digitAt: 1) printOn: aStream base: b."
	[ pos > 0 ]
		whileTrue: [ aStream nextPut: (ASTCharacter digitValue: (digits at: pos)).
			pos := pos - 1 ]
]

{ #category : #'as yet unclassified' }
ASTInteger >> quo: aNumber [
	"Refer to the comment in Number quo: "

	| ng quo |
	aNumber isInteger
		ifTrue: [ ng := self negative == aNumber negative == false.
			quo := (self
				digitDiv:
					(aNumber isSmallInteger
						ifTrue: [ aNumber abs ]
						ifFalse: [ aNumber ])
				neg: ng) at: 1.
			^ quo normalize ].
	^ aNumber adaptToInteger: self andSend: #quo:
]

{ #category : #'as yet unclassified' }
ASTInteger >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	| j |
	"Catches failure if LgInt replace primitive fails"
	j := repStart.
	start to: stop do: [ :i | 
		self digitAt: i put: (replacement digitAt: j).
		j := j + 1 ]
]

{ #category : #'as yet unclassified' }
ASTInteger >> rounded [
	"Refer to the comment in Number|rounded."

	
]

{ #category : #'as yet unclassified' }
ASTInteger >> timesRepeat: aBlock [
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| count |
	count := 1.
	[ count <= self ]
		whileTrue: [ aBlock value.
			count := count + 1 ]
]

{ #category : #'as yet unclassified' }
ASTInteger >> truncated [
	"Refer to the comment in Number|truncated."

	
]
