Class {
	#name : #PCInterval,
	#superclass : #PCSequenceableCollection,
	#instVars : [
		'start',
		'stop',
		'step'
	],
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
PCInterval class >> from: startInteger to: stopInteger [
	"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of 1."

	^ self basicNew setFrom: startInteger to: stopInteger by: 1
]

{ #category : #'as yet unclassified' }
PCInterval class >> from: startInteger to: stopInteger by: stepInteger [
	"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of stepNumber."

	^ self basicNew setFrom: startInteger to: stopInteger by: stepInteger
]

{ #category : #'as yet unclassified' }
PCInterval >> = anInterval [
	"Answer true if my species and anInterval species are equal, and
	if our starts, steps and sizes are equal."

	self species == anInterval species
		ifTrue: [ ^ start = anInterval first
				and: [ step = anInterval increment and: [ self size = anInterval size ] ] ]
		ifFalse: [ ^ false ]
]

{ #category : #'as yet unclassified' }
PCInterval >> add: newObject [
	"Adding to an Interval is not allowed."

	self shouldNotImplement
]

{ #category : #'as yet unclassified' }
PCInterval >> at: anInteger [
	"Answer the anInteger'th element."

	(anInteger >= 1 and: [ anInteger <= self size ])
		ifTrue: [ ^ start + (step * (anInteger - 1)) ]
		ifFalse: [ self errorSubscriptBounds: anInteger ]
]

{ #category : #'as yet unclassified' }
PCInterval >> at: anInteger put: anObject [
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'
]

{ #category : #'as yet unclassified' }
PCInterval >> collect: aBlock [
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do: [ :i | 
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step ].
	^ result
]

{ #category : #'as yet unclassified' }
PCInterval >> do: aBlock [
	| aValue |
	aValue := start.
	step < 0
		ifTrue: [ [ stop <= aValue ]
				whileTrue: [ aBlock value: aValue.
					aValue := aValue + step ] ]
		ifFalse: [ [ stop >= aValue ]
				whileTrue: [ aBlock value: aValue.
					aValue := aValue + step ] ]
]

{ #category : #'as yet unclassified' }
PCInterval >> first [
	"Refer to the comment in SequenceableCollection|first."

	^ start
]

{ #category : #'as yet unclassified' }
PCInterval >> hash [
	"Hash is reimplemented because = is implemented."

	^ (((start hash bitShift: 2) bitOr: stop hash) bitShift: 1)
		bitOr: self size
]

{ #category : #'as yet unclassified' }
PCInterval >> includes: aNumber [
	^ aNumber between: self first and: self last
]

{ #category : #'as yet unclassified' }
PCInterval >> increment [
	"Answer the receiver's interval increment."

	^ step
]

{ #category : #'as yet unclassified' }
PCInterval >> last [
	"Refer to the comment in SequenceableCollection|last."

	^ stop - ((stop - start) \\ step)
]

{ #category : #'as yet unclassified' }
PCInterval >> printOn: aStream [
	aStream nextPut: $(.
	start printOn: aStream.
	aStream nextPutAll: ' to: '.
	stop printOn: aStream.
	step ~= 1
		ifTrue: [ aStream nextPutAll: ' by: '.
			step printOn: aStream ].
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
PCInterval >> remove: newObject [
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'
]

{ #category : #'as yet unclassified' }
PCInterval >> setFrom: startInteger to: stopInteger by: stepInteger [
	start := startInteger.
	stop := stopInteger.
	step := stepInteger
]

{ #category : #'as yet unclassified' }
PCInterval >> size [
	step < 0
		ifTrue: [ start < stop
				ifTrue: [ ^ 0 ]
				ifFalse: [ ^ (stop - start) // step + 1 ] ]
		ifFalse: [ stop < start
				ifTrue: [ ^ 0 ]
				ifFalse: [ ^ (stop - start) // step + 1 ] ]
]

{ #category : #'as yet unclassified' }
PCInterval >> species [
	^ PCArray
]
