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
		'selectors'
	],
	#category : #'ASTSmalltalk-Compiler'
}
