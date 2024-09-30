Class {
	#name : 'AStackValueTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'three',
		'theAnswer',
		'threePlus',
		'fourPlus',
		'otherThree',
		'unbounded',
		'theArray',
		'sizeOf',
		'threeToSizeOf',
		'block1',
		'block2'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
AStackValueTest >> setUp [

	super setUp.

	unbounded := AStSmallInteger new.
	three := AStSmallInteger setValue: 3.
	otherThree := AStSmallInteger setValue: 3.
	threePlus := AStSmallInteger geThan: 3.
	fourPlus := AStSmallInteger geThan: 4.
	theAnswer := AStSmallInteger setValue: 42.
	theArray := Object new.
	sizeOf := AStSizeOf object: theArray.
	threeToSizeOf := (AStSmallInteger geThan: 3) leThan: sizeOf.
	block1 := AStBlock block: [ 3 + 4 ].
	block2 := AStBlock block: [ 34 * 2 ].
	" more cases to add "
]

{ #category : 'tests' }
AStackValueTest >> testMergeBlock [
	" test all the kinds of `AStBlock`s defined in the setUp "

	self assert: (block1 couldMergeIn: block2) equals: false.
	self assert: (block2 couldMergeIn: block1) equals: false.
	self assert: (block2 couldMergeIn: block2) equals: true.	
]

{ #category : 'tests' }
AStackValueTest >> testMergeSmallInteger [
	" test all the kinds of `AStSmallInteger`s defined in the setUp "

	self assert: three leThan equals: 3.
	self assert: three geThan equals: 3.
	self assert: (three couldMergeIn: three).
	self assert: (three mergeIn: theAnswer) leThan equals: 42.
	self assert: (three mergeIn: theAnswer) geThan equals: 3.
	self assert: (threePlus mergeIn: theAnswer) geThan equals: 3.
	self assert: (threePlus mergeIn: theAnswer) leThan equals: nil.
	self assert: (three delta: 1) equals: fourPlus.
	self assert: (threePlus mergeIn: theAnswer) equals: threePlus.
	self assert: (theAnswer mergeIn: threePlus) equals: threePlus.
	self assert: (three mergeIn: (three delta: 1)) equals: threePlus.
	self assert: ((three delta: 1) mergeIn: three) equals: threePlus.
	self assert: (threePlus mergeIn: (threePlus delta: 1)) equals: threePlus.
	self assert: ((threePlus delta: 1) mergeIn: threePlus) equals: threePlus.
	self assert: (otherThree mergeIn: (three delta: 1)) equals: unbounded.
	self assert: ((three delta: 1) mergeIn: otherThree) equals: unbounded
]
