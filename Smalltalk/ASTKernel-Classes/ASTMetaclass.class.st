Class {
	#name : #ASTMetaclass,
	#superclass : #ASTBehavior,
	#instVars : [
		'soleInstance'
	],
	#category : #'ASTKernel-Classes'
}

{ #category : #'as yet unclassified' }
ASTMetaclass >> classSide [
	^ self
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> initMethodDict: newMethodDict [
	"Initialize myself with the given method dictionary. Create but do not initialize my soleInstance."

	superclass := ASTClass.
	methodDict := newMethodDict.
	format := ASTClass format.	"all metaclasses have the same format as PClass"
	soleInstance := self basicNew
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> instanceSide [
	"Answer my only instance."

	^ soleInstance
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> isMeta [
	^ true
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> name [
	"Answer my name, either 'Metaclass' or the name of my class followed by ' class'."

	soleInstance
		ifNil: [ ^ 'Metaclass' ]
		ifNotNil: [ ^ soleInstance name , ' class' ]
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> new [
	"Each metaclass should have exactly one instance."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
ASTMetaclass >> soleInstance: aClass [
	soleInstance := aClass
]
