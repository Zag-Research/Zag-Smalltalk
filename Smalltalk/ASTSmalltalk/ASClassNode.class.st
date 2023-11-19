"
I represent a Class that contains a number of methods and has super-classes.
"
Class {
	#name : 'ASClassNode',
	#superclass : 'Object',
	#instVars : [
		'name',
		'classVariables',
		'methods'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'comparing' }
ASClassNode >> = other [
	^ name = other name
]

{ #category : 'initialization' }
ASClassNode >> from: aClass [

	name := aClass name.
	methods := OrderedCollection new.
	classVariables := aClass classPool keys.
]

{ #category : 'comparing' }
ASClassNode >> hash [
	^ name hash
]

{ #category : 'initialization' }
ASClassNode >> initialize [ 
	scope := Dictionary new
]

{ #category : 'accessing' }
ASClassNode >> lookup: symbol [

	^ scope
		  at: symbol
		  ifAbsent: [
		  ASClassVariable name: symbol "nestedScopeOf lookup: symbol" ] 
]

{ #category : 'accessing' }
ASClassNode >> methods [
^ methods
]

{ #category : 'accessing' }
ASClassNode >> name [

	^ name
]
