"
I explore 3 ways of getting constants from a class.

Turns out that on Pharo SharedPools are way efficient!
"
Class {
	#name : 'ZagConstantsStats',
	#superclass : 'Object',
	#instVars : [
		'constants'
	],
	#classVars : [
		'Stats'
	],
	#pools : [
		'ZagTestConstants'
	],
	#category : 'Zag-Exporter-Tests-Explore',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Explore'
}

{ #category : 'statistics' }
ZagConstantsStats class >> stats [

	<script>
	| instance repeat |
	Stats := Dictionary new.
	instance := self new.
	repeat := 100000.
	Stats at: #shared put: [ repeat timesRepeat: [ instance shared ] ] timeToRun. " 0.84s "
	Stats at: #ts put: [ repeat timesRepeat: [ instance ts ] ] timeToRun. "12.9s "
	Stats at: #ps put: [ repeat timesRepeat: [ instance ps ] ] timeToRun. " 8.4s "
	Stats inspect
]

{ #category : 'initialization' }
ZagConstantsStats >> initialize [

	constants := ZagTestConstants
]

{ #category : 'initialization' }
ZagConstantsStats >> ps [

	| result |
	result := 0.
	1000 timesRepeat: [ result := result + constants p1 + constants p2 + constants p3 + constants p4 + constants p5 ].
	^ result
]

{ #category : 'initialization' }
ZagConstantsStats >> shared [

	| result |
	result := 0.
	1000 timesRepeat: [ result := result + T1 + T2 + T3 + T4 + T5 ].
	^ result
]

{ #category : 'initialization' }
ZagConstantsStats >> ts [

	| result |
	result := 0.
	1000 timesRepeat: [ result := result + constants t1 + constants t2 + constants t3 + constants t4 + constants t5 ].
	^ result
]
