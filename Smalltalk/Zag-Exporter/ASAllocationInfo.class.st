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
		'format'
	],
	#classVars : [
		'HashCounter'
	],
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'adding' }
ASAllocationInfo class >> headerIVars: iVars indexed: nElements element: element weak: makeWeak constants: constants [

	nElements ifNotNil: [
		| arraySize |
		arraySize := nElements * (element ifNil: [ 8 ]) + 7 // 8.
		makeWeak ifTrue: [
			iVars + arraySize + 3 > constants maxHeaderLength ifTrue: [
				^ self new
					  format: constants formatExternalWeakWithPointers;
					  instVars: iVars;
					  footerLength: 3 ].
			^ self new
				  format: constants formatIndexedWeakWithPointers;
				  instVars: iVars;
				  extra: arraySize;
				  footerLength: 3 ].
		iVars == 0 ifTrue: [
			(nElements == 0 or: [ element = 1 and: [ nElements < 110 ] ])
				ifTrue: [
					^ self new
						  format: nElements;
						  extra: arraySize ].
			element ifNil: [
				nElements <= constants maxHeaderLength ifTrue: [
					^ self new
						  format: constants formatDirectIndexed;
						  extra: arraySize ] ] ].
		iVars + arraySize > (constants maxHeaderLength - 2) ifTrue: [
			^ self new
				  format: (element = Object
						   ifTrue: [ constants formatExternal ]
						   ifFalse: [ constants formatExternalNonObject ]);
				  instVars: iVars;
				  footerLength: 2 ].
		^ self new
			  format: (element = Object
					   ifTrue: [ constants formatIndexed ]
					   ifFalse: [ constants formatIndexedNonObject ]);
			  instVars: iVars;
			  extra: arraySize;
			  footerLength: 2 ].
	^ self new
		  format: constants formatNotIndexable;
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

{ #category : 'accessing' }
ASAllocationInfo >> header: classNumber into: anImage [

	| length |
	length := instVars + extra + footerLength.
	length < anImage constants maxHeaderLength ifFalse: [ self error: 'header too big' ].
	(anImage currentHeap hasSpaceFor: length) ifFalse: [
		anImage addSubHeap ].
	HashCounter := HashCounter + 1.
	^ anImage currentHeap
		  header:
			  length << 4 + anImage constants ageStatic << 8 + format << 24 + HashCounter << 16
			  + classNumber
		  size: length
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
ASAllocationInfo >> put: anObject into: anImage [

	| address class |
	class := anImage class: anObject class.
	address := self header: class into: anImage.
	anImage currentHeap
		at: address
		with: ((1 to: instVars) collect: [ :i | anObject instVarAt: i ])
		do: [ :v | v astImageObject: anImage ].
	extra > 0 ifTrue: [
	anImage currentHeap
		at: address + instVars
		with: anObject
		do: [ :v | v astImageObject: anImage ]].
	^ anImage pointer: address
]
