"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : #ASCompile,
	#superclass : #Object,
	#instVars : [
		'class'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #compilation }
ASCompile >> compileCode: sourceCode [
	^ self compileSource: (ASScanner new scanCode: sourceCode)
]

{ #category : #compilation }
ASCompile >> compileMethod: aMethod [
	class := aMethod methodClass.
	^ self compileCode: aMethod sourceCode
]
