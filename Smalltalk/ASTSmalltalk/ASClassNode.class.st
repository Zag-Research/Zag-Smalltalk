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

{ #category : #initialization }
ASClassNode >> from: aClass [ 
	name := aClass name.
	classVariables := aClass classPool keys.

]
