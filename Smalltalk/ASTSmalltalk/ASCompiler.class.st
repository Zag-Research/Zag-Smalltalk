"
I compile Smalltalk source from a Tonel file into a class with methods expressed as ASTs
"
Class {
	#name : #ASCompiler,
	#superclass : #Object,
	#instVars : [
		'class'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #compilation }
ASCompiler >> compileCode: sourceCode [
	^ self compileSource: (ASScanner new scanCode: sourceCode readStream;stream)
]

{ #category : #compilation }
ASCompiler >> compileMethod: aMethod [
	class := aMethod methodClass.
	^ self compileCode: aMethod sourceCode
]
