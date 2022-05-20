"
I represent a Class that contains a number of methods and has super-classes.
"
Class {
	#name : #ASClassNode,
	#superclass : #Object,
	#instVars : [
		'name',
		'classVariables'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASClassNode >> = other [
	^ name = other name
]

{ #category : #initialization }
ASClassNode >> from: aClass [

	name := aClass name.
	self halt.
	classVariables := aClass classPool keys
]

{ #category : #comparing }
ASClassNode >> hash [
	^ name hash
]

{ #category : #accessing }
ASClassNode >> name [

	^ name
]
