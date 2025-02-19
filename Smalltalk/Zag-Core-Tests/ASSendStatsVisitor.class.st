"
I calculate statistics about message sends
"
Class {
	#name : 'ASSendStatsVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'totalSends',
		'selfSends',
		'literalSends'
	],
	#category : 'Zag-Core-Tests-Explore',
	#package : 'Zag-Core-Tests',
	#tag : 'Explore'
}

{ #category : 'initialization' }
ASSendStatsVisitor >> initialize [
	super initialize.
	totalSends := selfSends := literalSends := 0
]

{ #category : 'accessing' }
ASSendStatsVisitor >> literalSends [

	^ literalSends
]

{ #category : 'accessing' }
ASSendStatsVisitor >> selfSends [

	^ selfSends
]

{ #category : 'accessing' }
ASSendStatsVisitor >> totalSends [

	^ totalSends
]

{ #category : 'visiting' }
ASSendStatsVisitor >> visitMessageNode: aMessageNode [

	totalSends := totalSends + 1.
	aMessageNode receiver isSelfOrSuperVariable ifTrue: [
		selfSends := selfSends + 1 ].
	aMessageNode receiver isLiteralNode ifTrue: [
		literalSends := literalSends + 1 ].
	^ super visitMessageNode: aMessageNode
]
