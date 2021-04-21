Class {
	#name : #PCPoint,
	#superclass : #PCObject,
	#instVars : [
		'x',
		'y'
	],
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
PCPoint class >> r: rho degrees: theta [
	"Answer an instance of me with polar coordinates rho and theta."

	^ self new setR: rho degrees: theta
]

{ #category : #'as yet unclassified' }
PCPoint class >> x: xValue y: yValue [
	"Answer an instance of me with the given coordinates."

	^ self new setX: xValue setY: yValue
]

{ #category : #'as yet unclassified' }
PCPoint >> * arg [
	"Answer a Point that is the product of the receiver and arg."

	arg isPoint
		ifTrue: [ ^ (x * arg x) @ (y * arg y) ].
	^ arg adaptToPoint: self andSend: #*
]

{ #category : #'as yet unclassified' }
PCPoint >> + arg [
	"Answer a Point that is the sum of the receiver and arg."

	arg isPoint
		ifTrue: [ ^ (x + arg x) @ (y + arg y) ].
	^ arg adaptToPoint: self andSend: #+
]

{ #category : #'as yet unclassified' }
PCPoint >> - arg [
	"Answer a Point that is the difference of the receiver and arg."

	arg isPoint
		ifTrue: [ ^ (x - arg x) @ (y - arg y) ].
	^ arg adaptToPoint: self andSend: #-
]

{ #category : #'as yet unclassified' }
PCPoint >> / arg [
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint
		ifTrue: [ ^ (x / arg x) @ (y / arg y) ].
	^ arg adaptToPoint: self andSend: #/
]

{ #category : #'as yet unclassified' }
PCPoint >> // arg [
	"Answer a Point that is the quotient of the receiver and arg."

	arg isPoint
		ifTrue: [ ^ (x // arg x) @ (y // arg y) ].
	^ arg adaptToPoint: self andSend: #//
]

{ #category : #'as yet unclassified' }
PCPoint >> = aPoint [
	self species = aPoint species
		ifTrue:
			[ ^ x = aPoint x and: [ y = aPoint y ]	"Refer to the comment in Object|=." ]
		ifFalse: [ ^ false ]
]

{ #category : #'as yet unclassified' }
PCPoint >> abs [
	"Answer a Point whose x and y are the absolute values of the receiver's x and y."

	^ x abs @ y abs
]

{ #category : #'as yet unclassified' }
PCPoint >> adaptToFloat: rcvr andSend: selector [
	"If I am involved in arithmetic with a Float, convert it to a Point."

	^ rcvr @ rcvr perform: selector with: self
]

{ #category : #'as yet unclassified' }
PCPoint >> adaptToInteger: rcvr andSend: selector [
	"If I am involved in arithmetic with an Integer, convert it to a Point."

	^ rcvr @ rcvr perform: selector with: self
]

{ #category : #'as yet unclassified' }
PCPoint >> asPoint [
	"Answer the receiver itself."

	^ self
]

{ #category : #'as yet unclassified' }
PCPoint >> crossProduct: aPoint [
	"Answer a number that is the cross product of the receiver and the argument, aPoint."

	^ x * aPoint y - (y * aPoint x)
]

{ #category : #'as yet unclassified' }
PCPoint >> degrees [
	"Answer the angle the receiver makes with origin in degrees. Right is 0; down is 90."

	| tan theta |
	x = 0
		ifTrue: [ y >= 0
				ifTrue: [ ^ 90.0 ]
				ifFalse: [ ^ 270.0 ] ]
		ifFalse: [ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			x >= 0
				ifTrue: [ y >= 0
						ifTrue: [ ^ theta radiansToDegrees ]
						ifFalse: [ ^ 360.0 + theta radiansToDegrees ] ]
				ifFalse: [ ^ 180.0 + theta radiansToDegrees ] ]
]

{ #category : #'as yet unclassified' }
PCPoint >> dist: aPoint [
	"Answer the distance between aPoint and the receiver."

	^ (aPoint - self) r
]

{ #category : #'as yet unclassified' }
PCPoint >> dotProduct: aPoint [
	"Answer a number that is the dot product of the receiver and the argument."

	^ x * aPoint x + (y * aPoint y)
]

{ #category : #'as yet unclassified' }
PCPoint >> hash [
	"Hash is reimplemented because = is implemented."

	^ (x hash bitShift: 2) bitXor: y hash
]

{ #category : #'as yet unclassified' }
PCPoint >> max: aPoint [
	"Answer a new Point whose x and y are the maximum of the receiver and the argument point x and y."

	^ (x max: aPoint x) @ (y max: aPoint y)
]

{ #category : #'as yet unclassified' }
PCPoint >> min: aPoint [
	"Answer a new Point whose x and y are the minimum of the receiver's and the argument point's x and y."

	^ (x min: aPoint x) @ (y min: aPoint y)
]

{ #category : #'as yet unclassified' }
PCPoint >> negated [
	"Answer a point whose x and y coordinates are the negatives of those of the receiver."

	^ x negated @ y negated
]

{ #category : #'as yet unclassified' }
PCPoint >> printOn: aStream [
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y printOn: aStream
]

{ #category : #'as yet unclassified' }
PCPoint >> r [
	"Answer the receiver's radius in polar coordinate system."

	^ (x * x + (y * y)) sqrt
]

{ #category : #'as yet unclassified' }
PCPoint >> rounded [
	"Answer a Point that is the receiver's x and y rounded. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [ y isInteger ])
		ifTrue: [ ^ self ].
	^ x rounded @ y rounded
]

{ #category : #'as yet unclassified' }
PCPoint >> setR: rho degrees: theta [
	| radians |
	radians := theta asFloat degreesToRadians.
	x := rho asFloat * radians cos.
	y := rho asFloat * radians sin
]

{ #category : #'as yet unclassified' }
PCPoint >> setX: xValue setY: yValue [
	x := xValue.
	y := yValue
]

{ #category : #'as yet unclassified' }
PCPoint >> theta [
	"Answer the angle the receiver makes with origin in radians. See degrees."

	| tan theta |
	x = 0
		ifTrue: [ y >= 0
				ifTrue: [ ^ 1.5708	"90.0 degreesToRadians" ]
				ifFalse: [ ^ 4.71239	"270.0 degreesToRadians" ] ]
		ifFalse: [ tan := y asFloat / x asFloat.
			theta := tan arcTan.
			x >= 0
				ifTrue: [ y >= 0
						ifTrue: [ ^ theta ]
						ifFalse: [ ^ 360.0 degreesToRadians + theta ] ]
				ifFalse: [ ^ 180.0 degreesToRadians + theta ] ]
]

{ #category : #'as yet unclassified' }
PCPoint >> truncated [
	"Answer a Point whose x and y coordinates are integers. Answer the receiver if its coordinates are already integral."

	(x isInteger and: [ y isInteger ])
		ifTrue: [ ^ self ].
	^ x truncated @ y truncated
]

{ #category : #'as yet unclassified' }
PCPoint >> x [
	^ x
]

{ #category : #'as yet unclassified' }
PCPoint >> y [
	^ y
]
