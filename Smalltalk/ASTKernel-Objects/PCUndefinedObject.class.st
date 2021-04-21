Class {
	#name : #PCUndefinedObject,
	#superclass : #PCObject,
	#category : #'ASTKernel-Objects'
}

{ #category : #'as yet unclassified' }
PCUndefinedObject class >> new [
	"There is a single unique instance of me."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> basicCopy [
	"There is the only one instance of me, so answer myself."

	^ self
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> ifNil: aBlock [
	"A convenient test, in conjunction with Object ifNil:"

	^ aBlock value
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> ifNil: nilBlock ifNotNil: ifNotNilBlock [
	"Evaluate the block for nil because I'm == nil"

	^ nilBlock value
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> ifNotNil: aBlock [
	"A convenient test, in conjunction with Object ifNotNil:"

	^ self
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> isNil [
	"Answer true if the receiver is nil."

	^ true
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> printOn: aStream [
	aStream nextPutAll: 'nil'
]

{ #category : #'as yet unclassified' }
PCUndefinedObject >> subclass: subclassName instanceVariableNames: instVarNames classVariableNames: classVarNames [
	^ PCClassBuilder new
		superclass: self;
		name: subclassName;
		instVarNames: instVarNames;
		classVariableNames: classVarNames;
		build
]
