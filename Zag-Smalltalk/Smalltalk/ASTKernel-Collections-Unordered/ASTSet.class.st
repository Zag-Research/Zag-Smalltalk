Class {
	#name : #ASTSet,
	#superclass : #ASTCollection,
	#instVars : [
		'tally',
		'array'
	],
	#category : #'ASTKernel-Collections-Unordered'
}

{ #category : #'instance creation' }
ASTSet class >> new [
	^ self new: 4
]

{ #category : #'as yet unclassified' }
ASTSet class >> new: nElements [
	"Create a Set large enough to hold nElements without growing."

	| initialSize |
	"make large enough size to hold nElements with some slop (see fullCheck)"
	nElements <= 0
		ifTrue: [ initialSize := 1 ]
		ifFalse: [ initialSize := (nElements + 1) * 4 // 3 ].
	^ self basicNew init: initialSize
]

{ #category : #comparing }
ASTSet >> = aSet [
	(aSet isKindOf: ASTSet)
		ifFalse: [ ^ false ].
	self size = aSet size
		ifFalse: [ ^ false ].
	self
		do: [ :each | 
			(aSet includes: each)
				ifFalse: [ ^ false ] ].
	^ true
]

{ #category : #'as yet unclassified' }
ASTSet >> add: newObject [
	"Add an element. User error instead of halt. go 10/1/97 09:33"

	| index |
	newObject == nil
		ifTrue: [ self error: 'Sets cannot meaningfully contain nil as an element' ].
	index := self findElementOrNil: newObject.
	(array at: index) == nil
		ifTrue: [ self atNewIndex: index put: newObject ].
	^ newObject
]

{ #category : #'as yet unclassified' }
ASTSet >> asArray [
	"Return an array whose elements are those of the receiver.  "

	| s |
	s := ASTWriteStream on: (ASTArray new: self size).
	self do: [ :el | s nextPut: el ].
	^ s contents
]

{ #category : #'as yet unclassified' }
ASTSet >> asSet [
	^ self
]

{ #category : #'as yet unclassified' }
ASTSet >> atNewIndex: index put: anObject [
	array at: index put: anObject.
	tally := tally + 1.
	self fullCheck
]

{ #category : #'as yet unclassified' }
ASTSet >> collect: aBlock [
	"Return a Set containing the result of evaluating aBlock for each element of this set."

	| newSet |
	tally = 0
		ifTrue: [ ^ ASTSet new: 2 ].
	newSet := ASTSet new: self size.
	array
		do: [ :each | 
			each == nil
				ifFalse: [ newSet add: (aBlock value: each) ] ].
	^ newSet
]

{ #category : #'as yet unclassified' }
ASTSet >> copy [
	^ self shallowCopy withArray: array shallowCopy
]

{ #category : #'as yet unclassified' }
ASTSet >> do: aBlock [
	tally = 0
		ifTrue: [ ^ self ].
	array
		do: [ :element | 
			element == nil
				ifFalse: [ aBlock value: element ] ]
]

{ #category : #'as yet unclassified' }
ASTSet >> findElementOrNil: anObject [
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."

	| index |
	index := self scanFor: anObject.
	index > 0
		ifTrue: [ ^ index ].	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!'
]

{ #category : #'as yet unclassified' }
ASTSet >> fixCollisionsFrom: index [
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| length oldIndex newIndex element |
	oldIndex := index.
	length := array size.
	[ oldIndex = length
		ifTrue: [ oldIndex := 1 ]
		ifFalse: [ oldIndex := oldIndex + 1 ].
	(element := self keyAt: oldIndex) == nil ]
		whileFalse: [ newIndex := self findElementOrNil: element.
			oldIndex = newIndex
				ifFalse: [ self swap: oldIndex with: newIndex ] ]
]

{ #category : #'as yet unclassified' }
ASTSet >> fullCheck [
	"Keep array at least 1/4 free for decent hash behavior"

	array size - tally < (array size // 4 max: 1)
		ifTrue: [ self grow ]
]

{ #category : #'as yet unclassified' }
ASTSet >> grow [
	"Grow the elements array and reinsert the old elements."

	| oldElements |
	oldElements := array.
	array := ASTArray new: array size + (array size max: 2).
	tally := 0.
	oldElements
		do: [ :each | 
			each == nil
				ifFalse: [ self noCheckAdd: each ] ]
]

{ #category : #'as yet unclassified' }
ASTSet >> includes: anObject [
	^ (array at: (self findElementOrNil: anObject)) ~~ nil
]

{ #category : #'as yet unclassified' }
ASTSet >> init: n [
	"Initialize array to an array size of n."

	array := ASTArray new: n.
	tally := 0
]

{ #category : #'as yet unclassified' }
ASTSet >> keyAt: index [
	"May be overridden by subclasses so that fixCollisions will work"

	^ array at: index
]

{ #category : #'as yet unclassified' }
ASTSet >> noCheckAdd: anObject [
	array at: (self findElementOrNil: anObject) put: anObject.
	tally := tally + 1
]

{ #category : #'as yet unclassified' }
ASTSet >> remove: oldObject ifAbsent: aBlock [
	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) == nil
		ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject
]

{ #category : #'as yet unclassified' }
ASTSet >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| element start finish |
	start := anObject hash \\ array size + 1.
	finish := array size.	"Search from (hash mod size) to the end."
	start to: finish do: [ :index | 
		((element := array at: index) == nil or: [ element = anObject ])
			ifTrue: [ ^ index ] ].	"Search from 1 to where we started."
	1 to: start - 1 do: [ :index | 
		((element := array at: index) == nil or: [ element = anObject ])
			ifTrue: [ ^ index ] ].
	^ 0	"No match AND no empty slot"
]

{ #category : #accessing }
ASTSet >> size [
	^ tally
]

{ #category : #'as yet unclassified' }
ASTSet >> swap: oneIndex with: otherIndex [
	"May be overridden by subclasses so that fixCollisions will work"

	array swap: oneIndex with: otherIndex
]

{ #category : #'as yet unclassified' }
ASTSet >> withArray: anArray [
	"private -- for use only in copy"

	array := anArray
]
