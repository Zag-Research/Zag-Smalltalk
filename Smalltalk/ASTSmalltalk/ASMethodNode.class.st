"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'class'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASMethodNode >> class: anASClassNode [ 
	class := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> selector [
	^ selector
]

{ #category : #accessing }
ASMethodNode >> selector: aString [ 
	selector := aString
]
