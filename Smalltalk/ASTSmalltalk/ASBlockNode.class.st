"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.
"
Class {
	#name : #ASBlockNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'method'
	],
	#category : #'ASTSmalltalk-AST'
}
