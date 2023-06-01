"
I represent a constrained variable. In addition to my value, I maintain the
structure of the constraint graph, the current dataflow graph, and various
parameters of interest to the DeltaBlue incremental constraint solver.

Instance variables:
	value			my value; changed by constraints, read by client <Object>
	constraints		normal constraints that reference me <Array of Constraint>
	determinedBy	the constraint that currently determines
					my value (or nil if there isn't one) <Constraint>
	walkStrength		my walkabout strength <Strength>
	stay			true if I am a planning-time constant <Boolean>
	mark			used by the planner to mark constraints <Number>
"
Class {
	#name : #PjDbVariable,
	#superclass : #Object,
	#instVars : [
		'value',
		'constraints',
		'determinedBy',
		'walkStrength',
		'stay',
		'mark'
	],
	#category : #'Zag-Benchmarking-DeltaBlue'
}

{ #category : #'instance creation' }
PjDbVariable class >> value: aValue [

	^self new value: aValue
]

{ #category : #access }
PjDbVariable >> addConstraint: aConstraint [
	"Add the given constraint to the set of all constraints that refer
	 to me."

	constraints add: aConstraint.
]

{ #category : #access }
PjDbVariable >> constraints [
	"Answer the set of constraints that refer to me."

	^constraints
]

{ #category : #access }
PjDbVariable >> determinedBy [
	"Answer the constraint that determines my value in the current
	 dataflow."

	^determinedBy
]

{ #category : #access }
PjDbVariable >> determinedBy: aConstraint [
	"Record that the given constraint determines my value in the current
	 data flow."

	determinedBy := aConstraint.
]

{ #category : #'initialize-release' }
PjDbVariable >> initialize [

	value := 0.
	constraints := OrderedCollection new: 2.
	determinedBy := nil.
	walkStrength := PjDbStrength absoluteWeakest.
	stay := true.
	mark := 0.
]

{ #category : #printing }
PjDbVariable >> longPrintOn: aStream [

	self shortPrintOn: aStream.
	aStream nextPutAll: '   Constraints: '.
	(constraints isEmpty)
		ifTrue: [aStream cr; tab; nextPutAll: 'none']
		ifFalse:
			[constraints do:
				[: c | aStream cr; tab. c shortPrintOn: aStream]].
	(determinedBy isNil) ifFalse:
		[aStream cr; nextPutAll: '   Determined by: '.
		 aStream cr; tab. determinedBy shortPrintOn: aStream].
	aStream cr.
]

{ #category : #access }
PjDbVariable >> mark [
	"Answer my mark value."

	^mark
]

{ #category : #access }
PjDbVariable >> mark: markValue [
	"Set my mark value."

	mark := markValue.
]

{ #category : #printing }
PjDbVariable >> printOn: aStream [

	self shortPrintOn: aStream
]

{ #category : #access }
PjDbVariable >> removeConstraint: c [
	"Remove all traces of c from this variable."

	constraints remove: c ifAbsent: [].
	(determinedBy == c) ifTrue: [determinedBy := nil].
]

{ #category : #changes }
PjDbVariable >> setValue: aValue [
	"Attempt to assign the given value to me using a strength of
	 #preferred."

	self setValue: aValue strength: #preferred.
]

{ #category : #changes }
PjDbVariable >> setValue: aValue strength: strengthSymbol [
	"Attempt to assign the given value to me using the given strength."

	| editConstraint |
	editConstraint := PjDbEditConstraint var: self strength: strengthSymbol.
	(editConstraint isSatisfied) ifTrue:
		[self value: aValue.
		 PjDbPlanner propagateFrom: self].
	editConstraint destroyConstraint.
]

{ #category : #printing }
PjDbVariable >> shortPrintOn: aStream [

	aStream nextPutAll: 'V(', self asOop printString, ', '.
	aStream nextPutAll: walkStrength printString, ', '.
	(stay isNil) ifFalse:
		[aStream nextPutAll: (stay ifTrue: ['stay, '] ifFalse: ['changing, '])].
	aStream nextPutAll: value printString.
	aStream nextPutAll: ')'.
	aStream cr.
]

{ #category : #access }
PjDbVariable >> stay [
	"Answer my stay flag."

	^stay
]

{ #category : #access }
PjDbVariable >> stay: aBoolean [
	"Set my stay flag."

	stay := aBoolean
]

{ #category : #access }
PjDbVariable >> value [
	"Answer my value."

	^value
]

{ #category : #access }
PjDbVariable >> value: anObject [
	"Set my value."

	value := anObject.
]

{ #category : #access }
PjDbVariable >> walkStrength [
	"Answer my walkabout strength in the current dataflow."

	^walkStrength
]

{ #category : #access }
PjDbVariable >> walkStrength: aStrength [
	"Set my walkabout strength in the current dataflow."

	walkStrength := aStrength.
]
