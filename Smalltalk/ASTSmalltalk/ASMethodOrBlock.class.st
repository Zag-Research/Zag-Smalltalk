Class {
	#name : #ASMethodOrBlock,
	#superclass : #Object,
	#instVars : [
		'arguments',
		'nestedScopeOf',
		'body',
		'locals'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASMethodOrBlock >> addStatement: anASStatement [

	body add: anASStatement
]

{ #category : #accessing }
ASMethodOrBlock >> arguments [
	^ arguments
]

{ #category : #accessing }
ASMethodOrBlock >> arguments: aCollection [ 
	arguments := aCollection asArray
]

{ #category : #accessing }
ASMethodOrBlock >> body [
	^ body
]

{ #category : #accessing }
ASMethodOrBlock >> body: anASNode [ 
	body := anASNode
]

{ #category : #initialization }
ASMethodOrBlock >> initialize [
	super initialize.
	locals := OrderedCollection new.

]

{ #category : #accessing }
ASMethodOrBlock >> locals [

	^ locals
]

{ #category : #accessing }
ASMethodOrBlock >> locals: aCollection [

	locals := aCollection asArray
]

{ #category : #accessing }
ASMethodOrBlock >> nestedScopeOf: anASClassNode [ 
	nestedScopeOf := anASClassNode
]

{ #category : #accessing }
ASMethodOrBlock >> scope [
	^ locals
]
