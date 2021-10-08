Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'arguments',
		'class',
		'nestedScopeOf',
		'body'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASMethodNode >> add: name [ 
	names add: name
]

{ #category : #accessing }
ASMethodNode >> arguments [
	^ arguments
]

{ #category : #accessing }
ASMethodNode >> arguments: aCollection [ 
	arguments := aCollection asArray
]

{ #category : #accessing }
ASMethodNode >> body: anASNode [ 
	body := anASNode
]

{ #category : #accessing }
ASMethodNode >> class: anASClassNode [ 
	class := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> nestedScopeOf: anASClassNode [ 
	nestedScopeOf := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> selector [
	^ selector
]

{ #category : #accessing }
ASMethodNode >> selector: aString [ 
	selector := aString
]
