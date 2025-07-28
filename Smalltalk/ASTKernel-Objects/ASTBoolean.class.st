Class {
	#name : 'ASTBoolean',
	#superclass : 'ASTObject',
	#category : 'ASTKernel-Objects',
	#package : 'ASTKernel-Objects'
}

{ #category : 'instance creation' }
ASTBoolean class >> new [
	"There is a single unique instance of each boolean."

	self cannotInstantiate
]

{ #category : 'reflective operations' }
ASTBoolean >> basicIdentityHash [
	"Answer the immediate hashcode."

	<primitive: 171>
	
]

{ #category : 'testing' }
ASTBoolean >> isBoolean [
	^ true
]

{ #category : 'copying' }
ASTBoolean >> shallowCopy [
	"There is the only one instance of me, so answer myself."

	^ self
]
