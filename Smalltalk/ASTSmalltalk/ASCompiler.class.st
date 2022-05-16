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
