Class {
	#name : #ASBstStats,
	#superclass : #Object,
	#instVars : [
		'nObjects',
		'totalDepth'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'as yet unclassified' }
ASBstStats class >> on: aCollection root: anInteger [

	^ self new
		on: aCollection root: anInteger depth: 0;
		result
]

{ #category : #initialization }
ASBstStats >> initialize [
	super initialize.
	nObjects := 0.
	totalDepth := 0.

]

{ #category : #'as yet unclassified' }
ASBstStats >> on: table root: parent depth: depth [
	parent = 0 ifTrue: [ ^ self ].
	nObjects := nObjects + 1.
	totalDepth := totalDepth + depth.
	self on: table root: (table at: parent) left depth: depth+1.
	self on: table root: (table at: parent) right depth: depth+1.

]

{ #category : #accessing }
ASBstStats >> result [
	^ 'average depth:',(totalDepth/nObjects asFloat) asString
]
