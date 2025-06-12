Class {
	#name : #ASTProcessList,
	#superclass : #ASTSequenceableCollection,
	#instVars : [
		'firstLink',
		'lastLink'
	],
	#category : #'ASTKernel-Processes'
}

{ #category : #'as yet unclassified' }
ASTProcessList >> add: aLink [
	"Add aLink to the end of the receiver's list. Answer aLink."

	^ self addLast: aLink
]

{ #category : #'as yet unclassified' }
ASTProcessList >> addLast: aLink [
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty
		ifTrue: [ firstLink := aLink ]
		ifFalse: [ lastLink nextLink: aLink ].
	lastLink := aLink.
	^ aLink
]

{ #category : #'as yet unclassified' }
ASTProcessList >> do: aBlock [
	"Evaluate the given block for each of my elements."

	| aLink |
	aLink := firstLink.
	[ aLink == nil ]
		whileFalse: [ aBlock value: aLink.
			aLink := aLink nextLink ]
]

{ #category : #'as yet unclassified' }
ASTProcessList >> first [
	"Answer the first element. Raise an error if I am empty."

	self emptyCheck.
	^ firstLink
]

{ #category : #'as yet unclassified' }
ASTProcessList >> isEmpty [
	^ firstLink == nil
]

{ #category : #'as yet unclassified' }
ASTProcessList >> remove: aLink ifAbsent: aBlock [
	"Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock."

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
				ifTrue: [ lastLink := tempLink ] ].
	aLink nextLink: nil.
	^ aLink
]

{ #category : #'as yet unclassified' }
ASTProcessList >> removeFirst [
	"Remove and answer the first element. Raise an error if I am empty."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [ firstLink := nil.
			lastLink := nil ]
		ifFalse: [ firstLink := oldLink nextLink ].
	oldLink nextLink: nil.
	^ oldLink
]

{ #category : #accessing }
ASTProcessList >> size [
	"Answer the number of elements I contain."

	| tally |
	tally := 0.
	self do: [ :each | tally := tally + 1 ].
	^ tally
]
