Class {
	#name : 'ASTOrderedCollection',
	#superclass : 'ASTSequenceableCollection',
	#instVars : [
		'array',
		'firstIndex',
		'lastIndex'
	],
	#category : 'ASTKernel-Collections-Ordered',
	#package : 'ASTKernel-Collections-Ordered'
}

{ #category : 'instance creation' }
ASTOrderedCollection class >> new [
	^ self new: 8
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection class >> new: anInteger [
	"If a subclass adds fields, then that subclass must reimplement new:."

	^ self basicNew setCollection: (ASTArray new: anInteger)
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> add: newObject [
	"Append newObject to me. Equivalent to addLast:. Answer newObject."

	lastIndex = array size
		ifTrue: [ self makeRoomAtLast ].
	lastIndex := lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> addFirst: newObject [
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1
		ifTrue: [ self makeRoomAtFirst ].
	firstIndex := firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject
]

{ #category : 'accessing' }
ASTOrderedCollection >> at: anInteger [
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	(anInteger < 1 or: [ anInteger + firstIndex - 1 > lastIndex ])
		ifTrue: [ self errorNoSuchElement ]
		ifFalse: [ ^ array at: anInteger + firstIndex - 1 ]
]

{ #category : 'accessing' }
ASTOrderedCollection >> at: anInteger put: anObject [
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	index := anInteger asInteger.
	(index < 1 or: [ index + firstIndex - 1 > lastIndex ])
		ifTrue: [ self errorNoSuchElement ]
		ifFalse: [ ^ array at: index + firstIndex - 1 put: anObject ]
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> collect: aBlock [
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection := self species new.
	self do: [ :each | newCollection add: (aBlock value: each) ].
	^ newCollection
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> copyFrom: startIndex to: endIndex [
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	| targetCollection |
	endIndex < startIndex
		ifTrue: [ ^ self species new: 0 ].
	targetCollection := self species new: endIndex + 1 - startIndex.
	startIndex to: endIndex do: [ :index | targetCollection add: (self at: index) ].
	^ targetCollection
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> copyReplaceFrom: start to: stop with: replacementCollection [
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection delta startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	delta := 0.
	startIndex := start.
	stopIndex := stop.
	start < 1
		ifTrue: [ startIndex := stopIndex := 0 ]
		ifFalse: [ startIndex > self size
				ifTrue: [ startIndex := stopIndex := self size + 1 ]
				ifFalse: [ (stopIndex < (startIndex - 1) or: [ stopIndex > self size ])
						ifTrue: [ self error: 'indices are out of bounds' ].
					delta := stopIndex - startIndex + 1 ] ].
	newOrderedCollection := self species
		new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [ :index | newOrderedCollection add: (self at: index) ].
	1 to: replacementCollection size do:
		[ :index | newOrderedCollection add: (replacementCollection at: index) ].
	stopIndex + 1 to: self size do: [ :index | newOrderedCollection add: (self at: index) ].
	^ newOrderedCollection
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> do: aBlock [
	"Override the superclass for performance reasons."

	| index |
	index := firstIndex.
	[ index <= lastIndex ]
		whileTrue: [ aBlock value: (array at: index).
			index := index + 1 ]
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> errorNoSuchElement [
	self
		error: 'attempt to index non-existent element in an ordered collection'
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> first [
	"Answer the first element. If the receiver is empty, create an errror
	message. This is a little faster than the implementation in the superclass."

	self emptyCheck.
	^ array at: firstIndex
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> grow [
	"Become larger. Typically, a subclass must override this method if it adds instance variables."

	| newArray |
	newArray := ASTArray new: self size + (array size max: 2).
	newArray
		replaceFrom: 1
		to: array size
		with: array
		startingAt: 1.
	array := newArray
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> insert: anObject before: spot [
	| index delta spotIndex |
	spotIndex := spot.
	delta := spotIndex - firstIndex.
	firstIndex = 1
		ifTrue: [ self makeRoomAtFirst.
			spotIndex := firstIndex + delta ].
	index := firstIndex := firstIndex - 1.
	[ index < (spotIndex - 1) ]
		whileTrue: [ array at: index put: (array at: index + 1).
			index := index + 1 ].
	array at: index put: anObject.
	^ anObject
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> last [
	"Answer the last element. If the receiver is empty, create an errror
	message. This is a little faster than the implementation in the superclass."

	self emptyCheck.
	^ array at: lastIndex
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> makeRoomAtFirst [
	| delta index |
	delta := array size - self size.
	delta = 0
		ifTrue: [ self grow.
			delta := array size - self size ].
	lastIndex = array size
		ifTrue: [ ^ self ].	"just in case we got lucky"
	index := array size.
	[ index > delta ]
		whileTrue: [ array at: index put: (array at: index - delta + firstIndex - 1).
			array at: index - delta + firstIndex - 1 put: nil.
			index := index - 1 ].
	firstIndex := delta + 1.
	lastIndex := array size
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> makeRoomAtLast [
	| newLast delta |
	newLast := self size.
	array size - self size = 0
		ifTrue: [ self grow ].
	(delta := firstIndex - 1) = 0
		ifTrue: [ ^ self ].	"we might be here under false premises or grow did the job for us"
	1 to: newLast do: [ :index | 
		array at: index put: (array at: index + delta).
		array at: index + delta put: nil ].
	firstIndex := 1.
	lastIndex := newLast
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> remove: oldObject ifAbsent: absentBlock [
	| index |
	index := firstIndex.
	[ index <= lastIndex ]
		whileTrue: [ oldObject = (array at: index)
				ifTrue: [ self removeIndex: index.
					^ oldObject ]
				ifFalse: [ index := index + 1 ] ].
	^ absentBlock value
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> removeFirst [
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."

	| firstObject |
	self emptyCheck.
	firstObject := array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^ firstObject
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> removeIndex: removedIndex [
	| index |
	index := removedIndex.
	[ index < lastIndex ]
		whileTrue: [ array at: index put: (array at: index + 1).
			index := index + 1 ].
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> removeLast [
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."

	| lastObject |
	self emptyCheck.
	lastObject := array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^ lastObject
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> select: aBlock [
	"Evaluate aBlock with each of my elements as the argument. Collect into a new collection like the receiver, only those elements for which aBlock evaluates to true. Override the superclass in order to use add:, not at:put:."

	| newCollection |
	newCollection := self species new.
	self
		do: [ :each | 
			(aBlock value: each)
				ifTrue: [ newCollection add: each ] ].
	^ newCollection
]

{ #category : 'as yet unclassified' }
ASTOrderedCollection >> setCollection: anArray [
	array := anArray.
	firstIndex := array size // 3 max: 1.
	lastIndex := firstIndex - 1
]

{ #category : 'accessing' }
ASTOrderedCollection >> size [
	^ lastIndex - firstIndex + 1
]
