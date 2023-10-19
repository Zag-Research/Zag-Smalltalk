"
I represent an argument to a method or a block
"
Class {
	#name : #ASParameter,
	#superclass : #ASLocal,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASParameter >> acceptVisitor: aVisitor [ 
	^ aVisitor visitArg: self
]

{ #category : #testing }
ASParameter >> isASArg [
	^true
]

{ #category : #testing }
ASParameter >> isImmediateThunk [
^ true
]

{ #category : #compiling }
ASParameter >> lookup: aGenerator [ 
	^ aGenerator lookupLocal: self
]
