"
I am the data field for elements on the compile-time stack.

I am used to determine what is possible to inline.

I hold limited compile-time values that can be tracked and merged to support optimization.

This can be a literal value.

Subclasses represent particular kinds of values.
"
Class {
	#name : 'AStackValue',
	#superclass : 'Object',
	#instVars : [
		'markedReferenced'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'instance creation' }
AStackValue class >> fromClass: aClass [

	aClass == SmallInteger ifTrue: [ ^ AStSmallInteger new ].
	aClass == Float ifTrue: [ ^ AStFloat new ].
	aClass == UndefinedClass ifTrue: [ ^ AStUndefinedClass new ].
	^ AStExactClass new dataClass: aClass
]

{ #category : 'instance creation' }
AStackValue class >> fromValue: anObject [

	| aClass |
	aClass := anObject class.
	aClass == SmallInteger ifTrue: [ ^ AStSmallInteger new setValue: anObject ].
	aClass == BlockClosure ifTrue: [ ^ AStBlock new block: anObject ].
	^ self fromClass: aClass
]

{ #category : 'API' }
AStackValue >> couldMergeIn: other [

	^ self subclassResponsibility
]

{ #category : 'API' }
AStackValue >> dataClass [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
AStackValue >> markReferenced [

	markedReferenced := true
]

{ #category : 'data flow instructions' }
AStackValue >> markUnreferenced [

	markedReferenced := false
]

{ #category : 'data flow instructions' }
AStackValue >> markedReferenced [

	^ markedReferenced
]

{ #category : 'API' }
AStackValue >> mergeIn: other [

	^ self subclassResponsibility
]
