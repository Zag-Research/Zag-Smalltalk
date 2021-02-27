Class {
	#name : #ASMethodNode,
	#superclass : #Object,
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
