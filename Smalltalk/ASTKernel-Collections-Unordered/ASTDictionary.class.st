Class {
	#name : #ASTDictionary,
	#superclass : #ASTSet,
	#category : #'ASTKernel-Collections-Unordered'
}

{ #category : #'as yet unclassified' }
ASTDictionary >> add: anAssociation [
	| index element |
	index := self findElementOrNil: anAssociation key.
	element := array at: index.
	element == nil
		ifTrue: [ self atNewIndex: index put: anAssociation ]
		ifFalse: [ element value: anAssociation value ].
	^ anAssociation
]

{ #category : #'as yet unclassified' }
ASTDictionary >> associationAt: key [
	^ self associationAt: key ifAbsent: [ self errorKeyNotFound ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> associationAt: key ifAbsent: aBlock [
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc
		ifTrue: [ ^ aBlock value ].
	^ assoc
]

{ #category : #'as yet unclassified' }
ASTDictionary >> associationsDo: aBlock [
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	super do: aBlock
]

{ #category : #'as yet unclassified' }
ASTDictionary >> at: key [
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [ self errorKeyNotFound ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> at: key ifAbsent: aBlock [
	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc
		ifTrue: [ ^ aBlock value ].
	^ assoc value
]

{ #category : #'as yet unclassified' }
ASTDictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."

	| index element |
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue:
			[ self atNewIndex: index put: (ASTAssociation key: key value: anObject) ]
		ifFalse: [ element value: anObject ].
	^ anObject
]

{ #category : #'as yet unclassified' }
ASTDictionary >> collect: aBlock [
	"Evaluate aBlock with each of my values as the argument. Collect the resulting values into a collection that is like me. Answer with the new collection."

	| newCollection |
	newCollection := ASTOrderedCollection new: self size.
	self do: [ :each | newCollection add: (aBlock value: each) ].
	^ newCollection
]

{ #category : #'as yet unclassified' }
ASTDictionary >> copy [
	"Must copy all my associations or later stores into either dictionary will effect both the original and the copy."

	^ self shallowCopy
		withArray:
			(array
				collect: [ :assoc | 
					assoc
						ifNil: [ nil ]
						ifNotNil: [ ASTAssociation key: assoc key value: assoc value ] ])
]

{ #category : #'as yet unclassified' }
ASTDictionary >> do: aBlock [
	super do: [ :assoc | aBlock value: assoc value ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> errorKeyNotFound [
	self error: 'key not found'
]

{ #category : #'as yet unclassified' }
ASTDictionary >> errorValueNotFound [
	self error: 'value not found'
]

{ #category : #'as yet unclassified' }
ASTDictionary >> includes: anObject [
	self
		do: [ :each | 
			anObject = each
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #'as yet unclassified' }
ASTDictionary >> includesKey: key [
	"Answer whether the receiver has a key equal to the argument, key."

	| index |
	index := self findElementOrNil: key.
	(array at: index) == nil
		ifTrue: [ ^ false ]
		ifFalse: [ ^ true ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> keyAt: index [
	"May be overridden by subclasses so that fixCollisions will work"

	| assn |
	assn := array at: index.
	assn == nil
		ifTrue: [ ^ nil ]
		ifFalse: [ ^ assn key ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> keyAtValue: value [
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil."

	^ self keyAtValue: value ifAbsent: [ self errorValueNotFound ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."

	self
		associationsDo: [ :association | 
			value = association value
				ifTrue: [ ^ association key ] ].
	^ exceptionBlock value
]

{ #category : #'as yet unclassified' }
ASTDictionary >> keys [
	"Answer a Set containing the receiver's keys."

	| aSet |
	aSet := ASTSet new: self size.
	self keysDo: [ :key | aSet add: key ].
	^ aSet
]

{ #category : #'as yet unclassified' }
ASTDictionary >> keysDo: aBlock [
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [ :association | aBlock value: association key ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> noCheckAdd: anObject [
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1
]

{ #category : #'as yet unclassified' }
ASTDictionary >> printOn: aStream [
	aStream nextPutAll: self class name , ' ('.
	self
		associationsDo: [ :element | 
			element printOn: aStream.
			aStream space ].
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
ASTDictionary >> remove: anObject [
	self shouldNotImplement
]

{ #category : #'as yet unclassified' }
ASTDictionary >> remove: anObject ifAbsent: exceptionBlock [
	self shouldNotImplement
]

{ #category : #'as yet unclassified' }
ASTDictionary >> removeKey: key [
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: key ifAbsent: [ self errorKeyNotFound ]
]

{ #category : #'as yet unclassified' }
ASTDictionary >> removeKey: key ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc == nil
		ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value
]

{ #category : #'as yet unclassified' }
ASTDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| element start finish |
	start := anObject hash \\ array size + 1.
	finish := array size.	"Search from (hash mod size) to the end."
	start to: finish do: [ :index | 
		((element := array at: index) == nil or: [ element key = anObject ])
			ifTrue: [ ^ index ] ].	"Search from 1 to where we started."
	1 to: start - 1 do: [ :index | 
		((element := array at: index) == nil or: [ element key = anObject ])
			ifTrue: [ ^ index ] ].
	^ 0	"No match AND no empty slot"
]

{ #category : #'as yet unclassified' }
ASTDictionary >> select: aBlock [
	"Evaluate aBlock with each of my values as the argument. Collect into a
	new dictionary, only those associations for which aBlock evaluates to
	true."

	| newCollection |
	newCollection := self species new.
	self
		associationsDo: [ :each | 
			(aBlock value: each value)
				ifTrue: [ newCollection add: each ] ].
	^ newCollection
]
