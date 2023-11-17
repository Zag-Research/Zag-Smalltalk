"
I am an actual context... I only exist if a context is required for this method
"
Class {
	#name : 'ASStackContext',
	#superclass : 'ASStackContextPlaceholder',
	#instVars : [
		'blocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASStackContext >> blocks: aCollection [
aCollection isCollection ifFalse: [ self halt ].
	blocks := aCollection
]

{ #category : 'instance creation' }
ASStackContext >> from: anASStack [

	rest := anASStack.
	blocks := #()
]

{ #category : 'initialization' }
ASStackContext >> initialize [ 
super initialize .
blocks := #()
]

{ #category : 'API' }
ASStackContext >> makeContextRequired [
^ self
]

{ #category : 'API' }
ASStackContext >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

	| index |
	index := blocks indexOf: anObject.
	index > 0 ifTrue: [ ^ blockLocal value: index - 1 ].
	^ rest
		  offset: blocks size
		  to: anObject
		  local: nil
		  stack: blockLocal
		  other: blockOther
]

{ #category : 'enumerating' }
ASStackContext >> requiredContextDo: aBlock [
 
	^ aBlock value: self	
]
