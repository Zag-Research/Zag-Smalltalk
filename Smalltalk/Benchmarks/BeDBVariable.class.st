"
I represent a constrained variable. In addition to my value, I maintain the
structure of the constraint graph, the current dataflow graph, and various
parameters of interest to the DeltaBlue incremental constraint solver.

Instance variables:
	value			my value; changed by constraints, read by client <Object>
	constraints		normal constraints that reference me <Array of Constraint>
	determinedBy	the constraint that currently determines
					my value (or nil if there isn't one) <Constraint>
	walkStrength		my walkabout strength <BeDBStrength>
	stay			true if I am a planning-time constant <Boolean>
	mark			used by the planner to mark constraints <Number>
"
Class {
	#name : #BeDBVariable,
	#superclass : #Object,
	#instVars : [
		'value',
		'constraints',
		'determinedBy',
		'walkStrength',
		'stay',
		'mark'
	],
	#category : #'Benchmarks-DeltaBlue'
}

{ #category : #'instance creation' }
BeDBVariable class >> new [

	^super new initialize
]

{ #category : #'instance creation' }
BeDBVariable class >> value: aValue [

	^(super new) initialize; value: aValue
]

{ #category : #access }
BeDBVariable >> addConstraint: aConstraint [
	"Add the given constraint to the set of all constraints that refer
	 to me."

	constraints add: aConstraint.
]

{ #category : #access }
BeDBVariable >> constraints [
	"Answer the set of constraints that refer to me."

	^constraints
]

{ #category : #access }
BeDBVariable >> determinedBy [
	"Answer the constraint that determines my value in the current
	 dataflow."

	^determinedBy
]

{ #category : #access }
BeDBVariable >> determinedBy: aConstraint [
	"Record that the given constraint determines my value in the current
	 data flow."

	determinedBy := aConstraint.
]

{ #category : #'initialize-release' }
BeDBVariable >> initialize [

	value := 0.
	constraints := OrderedCollection new: 2.
	determinedBy := nil.
	walkStrength := BeDBStrength absoluteWeakest.
	stay := true.
	mark := 0.
]

{ #category : #printing }
BeDBVariable >> longPrintOn: aStream [

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
BeDBVariable >> mark [
	"Answer my mark value."

	^mark
]

{ #category : #access }
BeDBVariable >> mark: markValue [
	"Set my mark value."

	mark := markValue.
]

{ #category : #printing }
BeDBVariable >> printOn: aStream [

	self shortPrintOn: aStream
]

{ #category : #access }
BeDBVariable >> removeConstraint: c [
	"Remove all traces of c from this variable."

	constraints remove: c ifAbsent: [].
	(determinedBy == c) ifTrue: [determinedBy := nil].
]

{ #category : #changes }
BeDBVariable >> setValue: aValue [
	"Attempt to assign the given value to me using a strength of
	 #preferred."

	self setValue: aValue strength: #preferred.
]

{ #category : #changes }
BeDBVariable >> setValue: aValue strength: strengthSymbol [
	"Attempt to assign the given value to me using the given strength."

	| editConstraint |
	editConstraint := BeDBEditConstraint var: self strength: strengthSymbol.
	(editConstraint isSatisfied) ifTrue:
		[self value: aValue.
		 BeDBPlanner propagateFrom: self].
	editConstraint destroyConstraint.
]

{ #category : #printing }
BeDBVariable >> shortPrintOn: aStream [

	aStream nextPutAll: 'V(', self asOop printString, ', '.
	aStream nextPutAll: walkStrength printString, ', '.
	(stay isNil) ifFalse:
		[aStream nextPutAll: (stay ifTrue: ['stay, '] ifFalse: ['changing, '])].
	aStream nextPutAll: value printString.
	aStream nextPutAll: ')'.
	aStream cr.
]

{ #category : #access }
BeDBVariable >> stay [
	"Answer my stay flag."

	^stay
]

{ #category : #access }
BeDBVariable >> stay: aBoolean [
	"Set my stay flag."

	stay := aBoolean
]

{ #category : #access }
BeDBVariable >> value [
	"Answer my value."

	^value
]

{ #category : #access }
BeDBVariable >> value: anObject [
	"Set my value."

	value := anObject.
]

{ #category : #access }
BeDBVariable >> walkStrength [
	"Answer my walkabout strength in the current dataflow."

	^walkStrength
]

{ #category : #access }
BeDBVariable >> walkStrength: aStrength [
	"Set my walkabout strength in the current dataflow."

	walkStrength := aStrength.
]
