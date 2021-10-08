Class {
	#name : #ASMethodOrBlock,
	#superclass : #Object,
	#instVars : [
		'names'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #initialization }
ASMethodOrBlock >> initialize [
	super initialize.
	names := OrderedCollection new.

]
