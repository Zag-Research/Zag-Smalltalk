Class {
	#name : #ASTCollection,
	#superclass : #ASTObject,
	#category : #'ASTKernel-Collections-Abstract'
}

{ #category : #'as yet unclassified' }
ASTCollection class >> with: anObject [
	"Answer an instance of me containing anObject."

	| newCollection |
	newCollection := self new.
	newCollection add: anObject.
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTCollection class >> with: firstObject with: secondObject [
	"Answer an instance of me containing the two arguments as elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTCollection class >> with: firstObject with: secondObject with: thirdObject [
	"Answer an instance of me containing the three arguments as elements."

	| newCollection |
	newCollection := self new.
	newCollection add: firstObject.
	newCollection add: secondObject.
	newCollection add: thirdObject.
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTCollection >> add: newObject [
	"Include newObject as one of my elements. Answer newObject. ArrayedCollections cannot respond to this message."

	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ASTCollection >> asArray [
	"Answer an Array whose elements are the elements of this collection. The order in which elements are added depends on the order in which this collection enumerates its elements. In the case of unordered collections, the ordering is not necessarily the same for multiple requests for the conversion."

	| result i |
	result := ASTArray new: self size.
	i := 0.
	self do: [ :each | result at: (i := i + 1) put: each ].
	^ result
]

{ #category : #'as yet unclassified' }
ASTCollection >> asByteArray [
	"Answer a ByteArray containing my elements."

	| result i |
	result := ASTByteArray new: self size.
	i := 0.
	self do: [ :each | result at: (i := i + 1) put: each ].
	^ result
]

{ #category : #'as yet unclassified' }
ASTCollection >> asSet [
	"Answer a Set whose elements are the unique elements of the receiver."

	| aSet |
	aSet := ASTSet new: self size.
	self do: [ :each | aSet add: each ].
	^ aSet
]

{ #category : #'as yet unclassified' }
ASTCollection >> collect: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect the resulting values into a collection like the receiver. Answer 
	the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [ :each | newCollection add: (aBlock value: each) ].
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTCollection >> detect: aBlock ifNone: exceptionBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true. If none 
	evaluate to true, then evaluate the argument, exceptionBlock."

	self
		do: [ :each | 
			(aBlock value: each)
				ifTrue: [ ^ each ] ].
	^ exceptionBlock value
]

{ #category : #'as yet unclassified' }
ASTCollection >> do: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ASTCollection >> emptyCheck [
	self isEmpty
		ifTrue: [ self errorEmptyCollection ]
]

{ #category : #'as yet unclassified' }
ASTCollection >> errorEmptyCollection [
	self error: 'this collection is empty'
]

{ #category : #'as yet unclassified' }
ASTCollection >> errorNotFound [
	self error: 'Object is not in the collection.'
]

{ #category : #'as yet unclassified' }
ASTCollection >> includes: anObject [
	"Answer whether anObject is one of the receiver's elements."

	self
		do: [ :each | 
			anObject = each
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #'as yet unclassified' }
ASTCollection >> isEmpty [
	"Answer whether the receiver contains any elements."

	^ self size = 0
]

{ #category : #'as yet unclassified' }
ASTCollection >> printOn: aStream [
	"Refer to the comment in Object|printOn:."

	aStream nextPutAll: self class name , ' ('.
	self
		do: [ :element | 
			element printOn: aStream.
			aStream space ].
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
ASTCollection >> remove: oldObject [
	"Remove oldObject as one of the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, create an error 
	notification."

	^ self remove: oldObject ifAbsent: [ self errorNotFound ]
]

{ #category : #'as yet unclassified' }
ASTCollection >> remove: oldObject ifAbsent: anExceptionBlock [
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. SequenceableCollections 
	cannot respond to this message."

	self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ASTCollection >> select: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for 
	which aBlock evaluates to true. Answer the new collection."

	| newCollection |
	newCollection := self species new.
	self
		do: [ :each | 
			(aBlock value: each)
				ifTrue: [ newCollection add: each ] ].
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTCollection >> size [
	"Answer how many elements the receiver contains."

	| count |
	count := 0.
	self do: [ :each | count := count + 1 ].
	^ count
]

{ #category : #'as yet unclassified' }
ASTCollection >> sum [
	"Answer the sum of the elements of this collection. If the collection is empty, answer zero."

	"Details: Use an arbitrary element of the collection as the initial value so this method will work for collections of any kind of object that understands + and -."

	| total seed |
	total := seed := self detect: [ :x | true ] ifNone: [ ^ 0 ].
	self do: [ :el | total := total + el ].
	^ total - seed	"subtract the seed value from the total"
]
