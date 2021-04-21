Class {
	#name : #ASTIdentityDictionary,
	#superclass : #ASTDictionary,
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
ASTIdentityDictionary >> keys [
	"Answer an array of the receiver's keys."

	| result |
	result := ASTOrderedCollection new: self size.
	self keysDo: [ :key | result add: key ].
	^ result asArray
]

{ #category : #'as yet unclassified' }
ASTIdentityDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| finish hash start element |
	finish := array size.
	finish > 4096
		ifTrue: [ hash := anObject identityHash * (finish // 4096) ]
		ifFalse: [ hash := anObject identityHash ].
	start := hash \\ array size + 1.	"Search from (hash mod size) to the end."
	start to: finish do: [ :index | 
		((element := array at: index) == nil or: [ element key == anObject ])
			ifTrue: [ ^ index ] ].	"Search from 1 to where we started."
	1 to: start - 1 do: [ :index | 
		((element := array at: index) == nil or: [ element key == anObject ])
			ifTrue: [ ^ index ] ].
	^ 0	"No match AND no empty slot"
]
