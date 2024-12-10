"
I contain several benchmarks to time Zag vs. Pharo
"
Class {
	#name : 'ASTimings',
	#superclass : 'Object',
	#classInstVars : [
		'stats'
	],
	#category : 'Zag-Core-Tests-Timing',
	#package : 'Zag-Core-Tests',
	#tag : 'Timing'
}

{ #category : 'accessing' }
ASTimings class >> stats [
	^ stats ifNil: [ stats := Dictionary new ]
]

{ #category : 'benchmarking' }
ASTimings class >> updateBenchmarks [
	" self stats removeKey: #fibonacci "
	<script>
	self methodDict keysDo: [ :key | 
		self stats
			at: key
			ifAbsentPut: [ [ self new perform: key ] timeToRun ] ].
	self stats inspect
]

{ #category : 'as yet unclassified' }
ASTimings >> allocate1000Megabytes [
" a stress test that Pablo cited at ESUG2022 "
	1 to: 1000 do: [ :e| Array new: 1024*1024 ]
]

{ #category : 'as yet unclassified' }
ASTimings >> dispatch1 [

	1 to: 1000000000 do: [ :e | (42 + e) yourself ]
]

{ #category : 'as yet unclassified' }
ASTimings >> dispatch3 [

	1 to: 1000000000 do: [ :e | 
		(e even
			 ifTrue: [ 42 ]
			 ifFalse: [ e & 2 = 0 ]) yourself ]
]

{ #category : 'as yet unclassified' }
ASTimings >> dispatch3b [

	1 to: 1000000000 do: [ :e | 
		(e even
			 ifTrue: [ 42 ]
			 ifFalse: [ e & 2 = 0 ]) asInteger ]
]

{ #category : 'as yet unclassified' }
ASTimings >> fibonacci [

	47 fibonacci
]

{ #category : 'as yet unclassified' }
ASTimings >> sieve [
	100000 benchmark
]
