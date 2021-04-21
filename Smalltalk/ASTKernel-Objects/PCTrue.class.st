Class {
	#name : #PCTrue,
	#superclass : #PCObject,
	#category : #'ASTKernel-Objects'
}

{ #category : #'as yet unclassified' }
PCTrue class >> new [
	"There is a single unique instance of each boolean."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
PCTrue >> & alternativeObject [
	"Answer true if both the receiver AND the argument are true. Unlike and:, the argument is always evaluted."

	^ alternativeObject
]

{ #category : #'as yet unclassified' }
PCTrue >> and: alternativeBlock [
	"Answer true if both the receiver AND the result of evaluating the given block are true. Only evaluate the given block if the receiver is true."

	^ alternativeBlock value
]

{ #category : #'as yet unclassified' }
PCTrue >> basicCopy [
	"There is the only one instance of me, so answer myself."

	^ self
]

{ #category : #'as yet unclassified' }
PCTrue >> ifFalse: falseBlock [
	"If the receiver is false, answer the result of evaluating the given block. Otherwise, answer nil. Raise an error notification if the receiver is not a boolean. Execution does not actually reach here because the expression is compiled in-line."

	^ nil
]

{ #category : #'as yet unclassified' }
PCTrue >> ifTrue: trueBlock [
	"If the receiver is true, answer the result of evaluating the given block. Otherwise, answer nil. Raise an error notification if the true is not a boolean. Execution does not actually reach here because the expression is compiled in-line."

	^ trueBlock value
]

{ #category : #'as yet unclassified' }
PCTrue >> ifTrue: trueBlock ifFalse: falseBlock [
	"If the receiver is true, answer the result of evaluating trueBlock. Otherwise, answer the result of evaluating falseBlock. Raise an error notification if the receiver is not a boolean. Execution does not actually reach here because this message is compiled in-line."

	^ trueBlock value
]

{ #category : #'as yet unclassified' }
PCTrue >> not [
	"Answer the negation of the receiver."

	^ false
]

{ #category : #'as yet unclassified' }
PCTrue >> or: alternativeBlock [
	"Answer true if either the receiver OR the argument are true. Only evaluate the given block if the receiver is false."

	^ true
]

{ #category : #'as yet unclassified' }
PCTrue >> printOn: aStream [
	aStream nextPutAll: 'true'
]

{ #category : #'as yet unclassified' }
PCTrue >> | aBoolean [
	"Answer true if either the receiver OR the argument are true. Unlike or:, the argument is always evaluted."

	^ true
]
