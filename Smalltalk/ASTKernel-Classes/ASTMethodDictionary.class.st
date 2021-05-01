Class {
	#name : #ASTMethodDictionary,
	#superclass : #ASTDictionary,
	#category : #'ASTKernel-Classes'
}

{ #category : #'as yet unclassified' }
ASTMethodDictionary class >> new: nElements [
	"Create a Dictionary large enough to hold nElements without growing."

	"NOTE: The basic size MUST be a power of 2. It is VITAL (see grow) that size gets doubled if nElements is a power of 2."

	| size |
	size := 1 bitShift: nElements highBit.
	^ (self basicNew: size) init: size
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> add: anAssociation [
	^ self at: anAssociation key put: anAssociation value
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> associationsDo: aBlock [
	| key |
	tally = 0
		ifTrue: [ ^ self ].
	1 to: self basicSize do: [ :i | 
		(key := self basicAt: i)
			ifNotNil: [ aBlock value: key -> (array at: i) ] ]
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> at: key ifAbsent: aBlock [
	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) == nil
		ifTrue: [ ^ aBlock value ].
	^ array at: index
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> at: key put: value [
	"Set the value at key to be value."

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) == nil
		ifTrue: [ tally := tally + 1.
			self basicAt: index put: key ].
	array at: index put: value.
	self fullCheck.
	^ value
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> copy [
	"Copy my values array."

	^ self shallowCopy withArray: array shallowCopy
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> do: aBlock [
	tally = 0
		ifTrue: [ ^ self ].
	1 to: self basicSize do: [ :i | (self basicAt: i) ifNotNil: [ aBlock value: (array at: i) ] ]
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> grow [
	| newSelf key |
	newSelf := self species new: self basicSize.	"This will double the size"
	1 to: self basicSize do: [ :i | 
		key := self basicAt: i.
		key == nil
			ifFalse: [ newSelf at: key put: (array at: i) ] ].
	self become: newSelf
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> includesKey: aSymbol [
	"This override assumes that pointsTo is a fast primitive."

	^ super pointsTo: aSymbol
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> keyAt: index [
	^ self basicAt: index
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key whose value equals the argument, value. If there is none, answer the result of evaluating exceptionBlock."

	| k |
	1 to: self basicSize do: [ :i | 
		value == (array at: i)
			ifTrue: [ (k := self basicAt: i) ifNotNil: [ ^ k ] ] ].
	^ exceptionBlock value
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> keysDo: aBlock [
	| key |
	tally = 0
		ifTrue: [ ^ self ].
	1 to: self basicSize do: [ :i | (key := self basicAt: i) ifNotNil: [ aBlock value: key ] ]
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> removeKey: key ifAbsent: errorBlock [
	"Pharo Candle does not support method removal."

	self shouldNotImplement
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| element start finish |
	start := anObject basicIdentityHash \\ array size + 1.
	finish := array size.	"Search from (hash mod size) to the end."
	start to: finish do: [ :index | 
		((element := self basicAt: index) == nil or: [ element == anObject ])
			ifTrue: [ ^ index ] ].	"Search from 1 to where we started."
	1 to: start - 1 do: [ :index | 
		((element := self basicAt: index) == nil or: [ element == anObject ])
			ifTrue: [ ^ index ] ].
	^ 0	"No match AND no empty slot"
]

{ #category : #'as yet unclassified' }
ASTMethodDictionary >> swap: oneIndex with: otherIndex [
	| element |
	element := self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	super swap: oneIndex with: otherIndex
]
