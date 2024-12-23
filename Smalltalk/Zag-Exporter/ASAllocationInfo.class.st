"
I know how to generate memory objects.

I parallel what is done by the AlocationInfo struct in heap.zig
"
Class {
	#name : 'ASAllocationInfo',
	#superclass : 'Object',
	#instVars : [
		'footerLength',
		'extra',
		'instVars',
		'format',
		'targetHeap',
		'address',
		'pointer'
	],
	#classVars : [
		'HashCounter'
	],
	#pools : [
		'ZagConstants'
	],
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'adding' }
ASAllocationInfo class >> headerIVars: iVars [

	^ self
		  headerIVars: iVars
		  indexed: nil
		  element: Object
		  weak: false
]

{ #category : 'adding' }
ASAllocationInfo class >> headerIVars: iVars indexed: nElements [

	^ self
		  headerIVars: iVars
		  indexed: nElements
		  element: Object
		  weak: false
]

{ #category : 'adding' }
ASAllocationInfo class >> headerIVars: iVars indexed: nElements element: element [

	^ self headerIVars: iVars indexed: nElements element: element weak: false
]

{ #category : 'adding' }
ASAllocationInfo class >> headerIVars: iVars indexed: nElements element: element weak: makeWeak [

	nElements ifNotNil: [
		| arraySize |
		arraySize := nElements * (element = Character ifTrue: [ 1 ] ifFalse: [ 8 ]) + 7 // 8.
		makeWeak ifTrue: [
			iVars + arraySize + 3 > MaxHeaderLength ifTrue: [
				^ self new
					  format: FormatExternalWeakWithPointers;
					  instVars: iVars;
					  footerLength: 3 ].
			^ self new
				  format: FormatIndexedWeakWithPointers;
				  instVars: iVars;
				  extra: arraySize;
				  footerLength: 3 ].
		iVars == 0 ifTrue: [
			(nElements == 0 or: [ element = Character and: [ nElements < 110 ] ])
				ifTrue: [
					^ self new
						  format: nElements;
						  extra: arraySize ].
			element = Object ifTrue: [
				nElements <= MaxHeaderLength ifTrue: [
					^ self new
						  format: FormatDirectIndexed;
						  extra: arraySize ] ] ].
		iVars + arraySize > (MaxHeaderLength - 2) ifTrue: [
			^ self new
				  format: (element = Object
						   ifTrue: [ FormatExternal ]
						   ifFalse: [ FormatExternalNonObject ]);
				  instVars: iVars;
				  footerLength: 2 ].
		^ self new
			  format: (element = Object
					   ifTrue: [ FormatIndexed ]
					   ifFalse: [ FormatIndexedNonObject ]);
			  instVars: iVars;
			  extra: arraySize;
			  footerLength: 2 ].
	^ self new
		  format: FormatNotIndexable;
		  instVars: iVars
]

{ #category : 'initialization' }
ASAllocationInfo class >> initialize [

	self reset
]

{ #category : 'initialization' }
ASAllocationInfo class >> reset [

	HashCounter := 0
]

{ #category : 'accessing' }
ASAllocationInfo >> address [
	^ address
]

{ #category : 'accessing' }
ASAllocationInfo >> extra: anObject [

	extra := anObject
]

{ #category : 'accessing' }
ASAllocationInfo >> footerLength: anObject [

	footerLength := anObject
]

{ #category : 'accessing' }
ASAllocationInfo >> format: fmt [ 

	format := fmt
]

{ #category : 'initialization' }
ASAllocationInfo >> initialize [

	super initialize.
	instVars := 0.
	footerLength := 0.
	extra := 0
]

{ #category : 'accessing' }
ASAllocationInfo >> instVars: n [

	instVars := n
]

{ #category : 'accessing' }
ASAllocationInfo >> pointer [

	^ pointer
]

{ #category : 'accessing' }
ASAllocationInfo >> put: anObject into: anImage [

	^ self
		  put: anObject
		  ivBlock: [ :i | (anObject instVarAt: i) encodedBy: anImage ]
		  indexBlock: [ :i | (anObject at: i) encodedBy: anImage ]
]

{ #category : 'accessing' }
ASAllocationInfo >> put: anObject into: anImage indexBlock: indexBlock [

	^ self
		  put: anObject
		  ivBlock: [ :i | (anObject instVarAt: i) encodedBy: anImage ]
		  indexBlock: indexBlock
]

{ #category : 'accessing' }
ASAllocationInfo >> put: anObject into: anImage ivBlock: ivBlock [

	^ self put: anObject ivBlock: ivBlock indexBlock: nil
]

{ #category : 'accessing' }
ASAllocationInfo >> put: anObject ivBlock: ivBlock indexBlock: indexBlock [

	targetHeap at: address size: instVars do: ivBlock.
	extra > 0 ifTrue: [
		targetHeap at: address + instVars size: anObject size do: indexBlock ].
	^ pointer
]

{ #category : 'accessing' }
ASAllocationInfo >> reserve: anObject in: anImage [

	| classindex |
	classindex := anImage classIndex: anObject class.
	^ self reserveClassIndex: classindex in: anImage
]

{ #category : 'accessing' }
ASAllocationInfo >> reserveClassIndex: classindex in: anImage [

	| length |
	length := instVars + extra + footerLength.
	length < MaxHeaderLength ifFalse: [ self error: 'header too big' ].
	targetHeap := anImage findHeapWithSpaceFor: length + 1.
	HashCounter := HashCounter + 1.
	address := targetHeap
		           header:
			           length << 4 + AgeStatic << 8 + format << 24
			           + HashCounter << 16 + classindex
		           size: length.
	^ pointer := anImage pointer: address
]
