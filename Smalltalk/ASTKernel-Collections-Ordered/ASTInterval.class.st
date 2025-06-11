Class {
	#name : 'ASTInterval',
	#superclass : 'ASTSequenceableCollection',
	#instVars : [
		'start',
		'stop',
		'step'
	],
	#category : 'ASTKernel-Collections-Ordered',
	#package : 'ASTKernel-Collections-Ordered'
}

{ #category : 'as yet unclassified' }
ASTInterval class >> from: startInteger to: stopInteger [
	"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of 1."

	^ self basicNew setFrom: startInteger to: stopInteger by: 1
]

{ #category : 'as yet unclassified' }
ASTInterval class >> from: startInteger to: stopInteger by: stepInteger [
	"Answer an instance of me, starting at startNumber, ending at stopNumber, and with an interval increment of stepNumber."

	^ self basicNew setFrom: startInteger to: stopInteger by: stepInteger
]

{ #category : 'comparing' }
ASTInterval >> = anInterval [
	"Answer true if my species and anInterval species are equal, and
	if our starts, steps and sizes are equal."

	self species == anInterval species
		ifTrue: [ ^ start = anInterval first
				and: [ step = anInterval increment and: [ self size = anInterval size ] ] ]
		ifFalse: [ ^ false ]
]

{ #category : 'as yet unclassified' }
ASTInterval >> add: newObject [
	"Adding to an Interval is not allowed."

	self shouldNotImplement
]

{ #category : 'accessing' }
ASTInterval >> at: anInteger [
	"Answer the anInteger'th element."

	(anInteger between: 1 and: self size)
		ifTrue: [ ^ start + (step * (anInteger - 1)) ]
		ifFalse: [ self errorSubscriptBounds: anInteger ]
]

{ #category : 'accessing' }
ASTInterval >> at: anInteger put: anObject [
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'
]

{ #category : 'as yet unclassified' }
ASTInterval >> collect: aBlock [
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do: [ :i | 
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step ].
	^ result
]

{ #category : 'as yet unclassified' }
ASTInterval >> do: aBlock [
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

{ #category : 'as yet unclassified' }
ASTInterval >> first [
	"Refer to the comment in SequenceableCollection|first."

	^ start
]

{ #category : 'comparing' }
ASTInterval >> hash [
	"Hash is reimplemented because = is implemented."

	^ (((start hash bitShift: 2) bitOr: stop hash) bitShift: 1)
		bitOr: self size
]

{ #category : 'as yet unclassified' }
ASTInterval >> includes: aNumber [
	^ aNumber between: self first and: self last
]

{ #category : 'as yet unclassified' }
ASTInterval >> increment [
	"Answer the receiver's interval increment."

	^ step
]

{ #category : 'as yet unclassified' }
ASTInterval >> last [
	"Refer to the comment in SequenceableCollection|last."

	^ stop - ((stop - start) \\ step)
]

{ #category : 'printing' }
ASTInterval >> printOn: aStream [
	aStream nextPut: $(.
	start printOn: aStream.
	aStream nextPutAll: ' to: '.
	stop printOn: aStream.
	step ~= 1
		ifTrue: [ aStream nextPutAll: ' by: '.
			step printOn: aStream ].
	aStream nextPut: $)
]

{ #category : 'as yet unclassified' }
ASTInterval >> remove: newObject [
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'
]

{ #category : 'as yet unclassified' }
ASTInterval >> setFrom: startInteger to: stopInteger by: stepInteger [
	start := startInteger.
	stop := stopInteger.
	step := stepInteger
]

{ #category : 'accessing' }
ASTInterval >> size [
	step < 0
		ifTrue: [ start < stop
				ifTrue: [ ^ 0 ]
				ifFalse: [ ^ (stop - start) // step + 1 ] ]
		ifFalse: [ stop < start
				ifTrue: [ ^ 0 ]
				ifFalse: [ ^ (stop - start) // step + 1 ] ]
]

{ #category : 'as yet unclassified' }
ASTInterval >> species [
	^ ASTArray
]
