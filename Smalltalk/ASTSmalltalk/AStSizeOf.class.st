"
I rempresent an integer that is know to be the size of some other object +/- some offset.

This can be used by `at:` or `at:put:` to be know to be safe.
"
Class {
	#name : 'AStSizeOf',
	#superclass : 'AStSmallInteger',
	#instVars : [
		'selfValue',
		'offset'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
AStSizeOf >> couldMergeIn: other [

	^ self class == other class and: [
		  selfValue == other selfValue and: [ offset = other offset ] ]
]

{ #category : 'accessing' }
AStSizeOf >> selfValue [

	^ selfValue
]

{ #category : 'accessing' }
AStSizeOf >> selfValue: anObject [

	selfValue := anObject
]
