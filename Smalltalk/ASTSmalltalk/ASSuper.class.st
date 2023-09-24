Class {
	#name : #ASSuper,
	#superclass : #ASExpression,
	#instVars : [
		'method',
		'superclass'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASSuper class >> method: aMethod [
	^ self new method: aMethod
]

{ #category : #'instance creation' }
ASSuper class >> method: aMethod superclass: aClass [

	^ self new method: aMethod; superclass: aClass
]

{ #category : #converting }
ASSuper >> asZagCompileObject [
	| results |
	results := OrderedCollection new.
	
	results add: 'c.ASSuper,'.
	results add: '":', self asZagLabel ,'",'.
	
^ results
]

{ #category : #accessing }
ASSuper >> method [

	^method
]

{ #category : #accessing }
ASSuper >> method: aMethod [

	method := aMethod
]

{ #category : #accessing }
ASSuper >> superclass [

	^superclass
]

{ #category : #accessing }
ASSuper >> superclass: aClass [

	superclass := aClass
]
