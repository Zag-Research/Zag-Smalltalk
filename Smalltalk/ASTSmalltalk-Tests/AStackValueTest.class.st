Class {
	#name : 'AStackValueTest',
	#superclass : 'TestCase',
	#instVars : [
		'three',
		'theAnswer',
		'threePlus',
		'fourPlus'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
AStackValueTest >> setUp [

	super setUp.

	three := AStSmallInteger setValue: 3.
	threePlus := AStSmallInteger geThan: 3.
	fourPlus := AStSmallInteger geThan: 4.
	theAnswer := AStSmallInteger setValue: 42
]

{ #category : 'tests' }
AStackValueTest >> testMergeSmallInteger [

	self assert: three leThan equals: 3.
	self assert: three geThan equals: 3.
	self assert: (three couldMergeIn: three).
	self assert: (three mergeIn: theAnswer) leThan equals: 42.
	self assert: (three mergeIn: theAnswer) geThan equals: 3.
	self assert: (threePlus mergeIn: theAnswer) geThan equals: 3.
	self assert: (threePlus mergeIn: theAnswer) leThan equals: nil.
	self assert: three + 1 equals: fourPlus.
	self assert: 1 + three equals: fourPlus.
	self assert: (threePlus mergeIn: theAnswer) equals: threePlus.
	self assert: (theAnswer mergeIn: threePlus) equals: threePlus
]
