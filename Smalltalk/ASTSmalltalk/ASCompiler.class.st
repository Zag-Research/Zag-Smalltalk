"
I implement the generic code for a Smalltalk -> AST compiler.

I have subclasses to accept various input formats, notably Pharo ASTs and sourcecode
"
Class {
	#name : #ASCompiler,
	#superclass : #Object,
	#instVars : [
		'classes',
		'methods',
		'selectors',
		'definingClass',
		'definingMethod',
		'currentScope'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #building }
ASCompiler >> buildFromClass: aClass method: aSymbol [

	self compileMethod: (self method: aSymbol fromClass: aClass class).
	self transitiveClosure
]

{ #category : #compiling }
ASCompiler >> compileMethod: aCompiledMethod [
	^ self subclassResponsibility 
]

{ #category : #compiling }
ASCompiler >> definingClass: aClass [

	definingClass := aClass.
	classes add: aClass.
	currentScope := aClass
]

{ #category : #accessing }
ASCompiler >> definingMethod [
	^ definingMethod
]

{ #category : #compiling }
ASCompiler >> definingMethod: aMethod [
	definingMethod := aMethod.
	methods add: aMethod
]

{ #category : #compiling }
ASCompiler >> inScope: anASMethodOrBlock do: aFullBlockClosure [ 
	| savedScope |
	savedScope := currentScope.
	currentScope := anASMethodOrBlock.
	aFullBlockClosure ensure: [ currentScope := savedScope ]
]

{ #category : #initialization }
ASCompiler >> initialize [

	super initialize.
	classes := Set new.
	selectors := Set new.
	methods := Set new.
]

{ #category : #building }
ASCompiler >> literal: anASLiteral isSent: aString [ 
	self flag: #todo
]

{ #category : #building }
ASCompiler >> method: aSymbol fromClass: aClass [

	^ aClass >> aSymbol
]

{ #category : #building }
ASCompiler >> self: anASSelf isSent: aString [ 
	self flag: #todo
]

{ #category : #building }
ASCompiler >> transitiveClosure [
	self flag: #ToDo
]

{ #category : #building }
ASCompiler >> unknownObjectIsSent: aString [ 
	self flag: #todo
]
