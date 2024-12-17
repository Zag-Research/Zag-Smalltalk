Class {
	#name : 'ASTLinkedList',
	#superclass : 'ASTSequenceableCollection',
	#instVars : [
		'firstLink',
		'lastLink'
	],
	#category : 'ASTKernel-Collections-Ordered',
	#package : 'ASTKernel-Collections-Ordered'
}

{ #category : 'as yet unclassified' }
ASTLinkedList class >> new: anInt [
	"LinkedList don't need capacity"

	^ self new
]

{ #category : 'as yet unclassified' }
ASTLinkedList class >> newFrom: aCollection [
	"Answer an instance with same elements as aCollection."

	^ self new
		addAll: aCollection;
		yourself
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> add: aLinkOrObject [
	"Add aLink to the end of the receiver's list. Answer aLink."

	^ self addLast: aLinkOrObject
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> add: link after: otherLinkOrObject [
	"Add otherLink  after link in the list. Answer aLink."

	| otherLink savedLink |
	otherLink := self linkAt: (self indexOf: otherLinkOrObject).
	^ self add: link afterLink: otherLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> add: aLinkOrObject afterLink: otherLink [
	"Add otherLink  after link in the list. Answer aLink."

	| savedLink aLink |
	lastLink == otherLink
		ifTrue: [ ^ self addLast: aLinkOrObject ].
	savedLink := otherLink nextLink.
	aLink := aLinkOrObject asLink.
	otherLink nextLink: aLink.
	aLink nextLink: savedLink.
	^ aLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> add: link before: otherLinkOrObject [
	"Add otherLink  after link in the list. Answer aLink."

	| otherLink savedLink |
	otherLink := self linkAt: (self indexOf: otherLinkOrObject).
	^ self add: link beforeLink: otherLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> add: aLinkOrObject beforeLink: otherLink [
	| currentLink |
	firstLink == otherLink
		ifTrue: [ ^ self addFirst: aLinkOrObject ].
	currentLink := firstLink.
	[ currentLink == nil ]
		whileFalse: [ currentLink nextLink == otherLink
				ifTrue: [ | aLink |
					aLink := aLinkOrObject asLink.
					aLink nextLink: currentLink nextLink.
					currentLink nextLink: aLink.
					^ aLink ].
			currentLink := currentLink nextLink ].
	^ self errorNotFound: otherLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> addFirst: aLinkOrObject [
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	| aLink |
	aLink := aLinkOrObject asLink.
	self isEmpty
		ifTrue: [ lastLink := aLink ].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^ aLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> addLast: aLinkOrObject [
	"Add aLink to the end of the receiver's list. Answer aLink."

	| aLink |
	aLink := aLinkOrObject asLink.
	self isEmpty
		ifTrue: [ firstLink := aLink ]
		ifFalse: [ lastLink nextLink: aLink ].
	lastLink := aLink.
	^ aLink
]

{ #category : 'accessing' }
ASTLinkedList >> at: index [
	^ (self linkAt: index) value
]

{ #category : 'accessing' }
ASTLinkedList >> at: index put: anObject [
	^ self
		at: index
		putLink: (self linkOf: anObject ifAbsent: [ anObject asLink ])
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> at: index putLink: aLink [
	| previousLink nextLink |
	"Please don't put a link which is already in the list, or you will create an infinite loop"
	(self validIndex: index)
		ifTrue: [ ^ self errorOutOfBounds ].
	index = 1
		ifTrue: [ aLink nextLink: self firstLink nextLink.
			firstLink := aLink.
			aLink nextLink ifNil: [ lastLink := aLink ].
			^ aLink ].
	previousLink := self linkAt: index - 1.
	nextLink := previousLink nextLink nextLink.
	nextLink
		ifNil: [ aLink nextLink: self lastLink ]
		ifNotNil: [ aLink nextLink: nextLink ].
	previousLink nextLink: aLink.
	nextLink
		ifNil: [ lastLink := aLink.
			aLink nextLink: nil ].
	^ aLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> collect: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| aLink newCollection |
	newCollection := self class new.
	aLink := firstLink.
	[ aLink isNil ]
		whileFalse: [ newCollection add: (aBlock value: aLink value).
			aLink := aLink nextLink ].
	^ newCollection
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> copyWith: newElement [
	^ self copy
		add: newElement;
		yourself
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> copyWithout: oldElement [
	| newInst |
	newInst := self class new.
	self
		do: [ :each | 
			each = oldElement
				ifFalse: [ newInst add: each ] ].
	^ newInst
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> do: aBlock [
	| aLink |
	aLink := firstLink.
	[ aLink == nil ]
		whileFalse: [ aBlock value: aLink value.
			aLink := aLink nextLink ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> first [
	"Answer the first link. Create an error notification if the receiver is 
	empty."

	^ self firstLink value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> firstLink [
	"Answer the first link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^ firstLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer the 	result of evaluating the argument, exceptionBlock."

	| currentLink index |
	currentLink := self linkAt: start ifAbsent: [ nil ].
	index := start.
	[ currentLink isNil ]
		whileFalse: [ currentLink value = anElement value
				ifTrue: [ ^ index ].
			currentLink := currentLink nextLink.
			index := index + 1 ].
	^ exceptionBlock value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> isEmpty [
	^ firstLink isNil
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> last [
	"Answer the last link. Create an error notification if the receiver is 
	empty."

	^ self lastLink value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> lastLink [
	"Answer the last link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^ lastLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> linkAt: index [
	^ self linkAt: index ifAbsent: [ self errorSubscriptBounds: index ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> linkAt: index ifAbsent: errorBlock [
	| i |
	i := 0.
	self
		linksDo: [ :link | 
			(i := i + 1) = index
				ifTrue: [ ^ link ] ].
	^ errorBlock value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> linkOf: anObject [
	^ self linkOf: anObject ifAbsent: [ self error: 'No such element' ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> linkOf: anObject ifAbsent: errorBlock [
	self
		linksDo: [ :el | 
			el value = anObject
				ifTrue: [ ^ el ] ].
	^ errorBlock value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> linksDo: aBlock [
	| aLink |
	aLink := firstLink.
	[ aLink isNil ]
		whileFalse: [ aBlock value: aLink.
			aLink := aLink nextLink ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> postCopy [
	| aLink |
	super postCopy.
	firstLink ifNotNil: [ aLink := firstLink := firstLink copy.
			[ aLink nextLink isNil ]
				whileFalse: [ aLink nextLink: (aLink := aLink nextLink copy) ].
			lastLink := aLink ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> remove: aLinkOrObject ifAbsent: aBlock [
	"Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock."

	| link |
	link := self linkOf: aLinkOrObject ifAbsent: [ ^ aBlock value ].
	self removeLink: link ifAbsent: [ ^ aBlock value ].
	^ aLinkOrObject
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeAll [
	"Implementation note: this has to be fast"

	firstLink := lastLink := nil
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeAllSuchThat: aBlock [
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  For LinkedLists, it's safe to use do:."

	self
		do: [ :each | 
			(aBlock value: each)
				ifTrue: [ self remove: each ] ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeFirst [
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [ firstLink := nil.
			lastLink := nil ]
		ifFalse: [ firstLink := oldLink nextLink ].
	oldLink nextLink: nil.
	^ oldLink value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeLast [
	"Remove the receiver's last element and answer it. If the receiver is 
	empty, create an error notification."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [ firstLink := nil.
			lastLink := nil ]
		ifFalse: [ aLink := firstLink.
			[ aLink nextLink == oldLink ]
				whileFalse: [ aLink := aLink nextLink ].
			aLink nextLink: nil.
			lastLink := aLink ].
	oldLink nextLink: nil.
	^ oldLink value
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeLink: aLink [
	^ self removeLink: aLink ifAbsent: [ self error: 'no such method!' ]
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> removeLink: aLink ifAbsent: aBlock [
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [ firstLink := aLink nextLink.
			aLink == lastLink
				ifTrue: [ lastLink := nil ] ]
		ifFalse: [ tempLink := firstLink.
			[ tempLink == nil
				ifTrue: [ ^ aBlock value ].
			tempLink nextLink == aLink ]
				whileFalse: [ tempLink := tempLink nextLink ].
			tempLink nextLink: aLink nextLink.
			aLink == lastLink
				ifTrue: [ lastLink := tempLink ] ].	"Not nilling the link enables us to delete while iterating"	"aLink nextLink: nil."
	^ aLink
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> species [
	^ ASTArray
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> swap: ix1 with: ix2 [
	"Reimplemented, super would create an infinite loop"

	| minIx maxIx link1Prev link2Prev link1 link2 link1Next link2Next newLink2Next |
	((self validIndex: ix1) and: [ self validIndex: ix2 ])
		ifFalse: [ ^ self errorOutOfBounds ].	"Get edge case out of the way"
	ix1 = ix2
		ifTrue: [ ^ self ].	"Sort indexes to make boundary-checks easier"
	minIx := ix1 min: ix2.
	maxIx := ix2 max: ix1.
	link1Prev := minIx = 1
		ifFalse: [ self linkAt: minIx - 1 ].
	link1 := link1Prev
		ifNotNil: [ link1Prev nextLink ]
		ifNil: [ self linkAt: minIx ].
	link1Next := link1 nextLink.
	link2Prev := self linkAt: maxIx - 1.
	link2 := link2Prev nextLink.
	link2Next := link2 nextLink.	"Link at start being swapped"
	link1 = firstLink
		ifTrue: [ firstLink := link2 ]
		ifFalse: [ link1Prev nextLink: link2 ].	"Link at end being swapped"
	link2 = lastLink
		ifTrue: [ lastLink := link1 ]
		ifFalse: [  ].	"Links  being swapped adjacent"
	newLink2Next := link1 nextLink = link2
		ifTrue: [ link1 ]
		ifFalse: [ link2Prev nextLink: link1.
			link1Next ].
	link1 nextLink: link2Next.
	link2 nextLink: newLink2Next
]

{ #category : 'as yet unclassified' }
ASTLinkedList >> validIndex: index [
	^ index > 0 and: [ index <= self size ]
]
