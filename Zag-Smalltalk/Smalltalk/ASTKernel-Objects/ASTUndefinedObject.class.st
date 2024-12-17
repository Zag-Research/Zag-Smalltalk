Class {
	#name : 'ASTUndefinedObject',
	#superclass : 'ASTObject',
	#category : 'ASTKernel-Objects',
	#package : 'ASTKernel-Objects'
}

{ #category : 'instance creation' }
ASTUndefinedObject class >> new [
	"There is a single unique instance of me."

	self cannotInstantiate
]

{ #category : 'reflective operations' }
ASTUndefinedObject >> basicIdentityHash [
	"Answer the immediate hashcode."

	<primitive: 171>
	
]

{ #category : 'testing' }
ASTUndefinedObject >> ifNil: aBlock [
	"A convenient test, in conjunction with Object ifNil:
	Do not override - will be inlined"

	^ aBlock value
]

{ #category : 'testing' }
ASTUndefinedObject >> ifNil: nilBlock ifNotNil: ifNotNilBlock [
	"Evaluate the block for nil because I'm == nil
	Do not override - will be inlined"

	^ nilBlock value
]

{ #category : 'testing' }
ASTUndefinedObject >> ifNotNil: aBlock [
	"A convenient test, in conjunction with Object ifNotNil:
	Do not override - will be inlined"

	^ self
]

{ #category : 'testing' }
ASTUndefinedObject >> ifNotNil: ifNotNilBlock ifNil: nilBlock [
	"If the receiver is not nil, pass it as argument to the ifNotNilBlock block. else execute the nilBlock block
	Do not override - will be inlined"

	^ nilBlock value
]

{ #category : 'testing' }
ASTUndefinedObject >> isNil [
	"Answer true if the receiver is nil."

	^ true
]

{ #category : 'printing' }
ASTUndefinedObject >> printOn: aStream [
	aStream nextPutAll: 'nil'
]

{ #category : 'copying' }
ASTUndefinedObject >> shallowCopy [
	"There is the only one instance of me, so answer myself."

	^ self
]

{ #category : 'as yet unclassified' }
ASTUndefinedObject >> subclass: subclassName instanceVariableNames: instVarNames classVariableNames: classVarNames [
	^ ASTClassBuilder new
		superclass: self;
		name: subclassName;
		instVarNames: instVarNames;
		classVariableNames: classVarNames;
		build
]
