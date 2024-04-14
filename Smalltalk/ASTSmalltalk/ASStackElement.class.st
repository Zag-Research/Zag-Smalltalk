"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackElement',
	#superclass : 'ASStack',
	#instVars : [
		'top',
		'rest'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASStackElement class >> from: anASStack [
	^ self basicNew from: anASStack 
]

{ #category : 'API' }
ASStackElement >> addBlockClosure: aCollection [ 
	self shouldBeImplemented.
]

{ #category : 'accessing' }
ASStackElement >> at: aSymbol put: anObject [
	rest at: aSymbol put: anObject 
]

{ #category : 'accessing' }
ASStackElement >> depth [

	^ 1 + rest depth
]

{ #category : 'accessing' }
ASStackElement >> drop [
	^ rest
]

{ #category : 'removing' }
ASStackElement >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : 'accessing' }
ASStackElement >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : 'instance creation' }
ASStackElement >> from: anASStack [

	rest := anASStack 
]

{ #category : 'testing' }
ASStackElement >> isEmpty [

	^ false
]

{ #category : 'API' }
ASStackElement >> makeContextRequired [
	rest := rest makeContextRequired.
	^ self
]

{ #category : 'API' }
ASStackElement >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

	^ rest
		  offset: anInteger + 1
		  to: anObject
		  local: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'enumerating' }
ASStackElement >> requiredContextDo: aBlock [ 
	^ rest requiredContextDo: aBlock
]

{ #category : 'API' }
ASStackElement >> stackNames: aCollection [

	aCollection ifEmpty: [ ^ self ].
	^ ASStackNamed new
		  from: (rest stackNames: aCollection allButFirst);
		  name: aCollection first;
		  top: top
]

{ #category : 'accessing' }
ASStackElement >> top [
^ top
]

{ #category : 'accessing' }
ASStackElement >> top: value [

value assertIsValue .
	top := value
]
