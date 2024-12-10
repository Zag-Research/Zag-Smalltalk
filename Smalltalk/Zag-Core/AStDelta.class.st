"
I am a delta of an `AStSmallInteger`.

I am only usefully mergeable with my originating `AStSmallInteger` in which case I produce an open range from that value.
"
Class {
	#name : 'AStDelta',
	#superclass : 'AStSmallInteger',
	#instVars : [
		'origin'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'API' }
AStDelta >> mergeIn: other [

	other == origin ifTrue: [ ^ self privateMergeIn: other ].
	^ AStSmallInteger new
]

{ #category : 'accessing' }
AStDelta >> origin: anAStSmallInteger [

	origin := anAStSmallInteger
]
