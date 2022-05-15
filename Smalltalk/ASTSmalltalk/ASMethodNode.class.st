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

{ #category : #comparing }
ASMethodNode >> = other [

	^ self class = other class and: [ 
		  self selector = other selector and: [ 
			  self className = other className and: [ super = other ] ] ]
]

{ #category : #accessing }
ASMethodNode >> class: anASClassNode [ 
	class := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> className [
	^ class name
]

{ #category : #printing }
ASMethodNode >> printOn: s [

	s << self class name << ' new class: '.
	class name printOn: s.
	s << '; selector: '.
	selector printOn: s.
	super printOn: s
]

{ #category : #accessing }
ASMethodNode >> selector [
	^ selector
]

{ #category : #accessing }
ASMethodNode >> selector: aString [ 
	selector := aString
]
