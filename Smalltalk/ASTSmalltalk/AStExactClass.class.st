"
I represent an object on the stack that we know nothing about.

Because we can make no assumptions, I can represent anything.
"
Class {
	#name : 'AStExactClass',
	#superclass : 'AStackValue',
	#instVars : [
		'dataClass'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
AStExactClass >> couldMergeIn: other [

	^ self dataClass == other dataClass
]

{ #category : 'as yet unclassified' }
AStExactClass >> dataClass [

	^ dataClass
]

{ #category : 'as yet unclassified' }
AStExactClass >> dataClass: aClass [

	dataClass := aClass
]
