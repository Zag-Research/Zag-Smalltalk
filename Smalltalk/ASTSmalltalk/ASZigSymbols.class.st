Class {
	#name : #ASZigSymbols,
	#superclass : #Dictionary,
	#instVars : [
		'index'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #accessing }
ASZigSymbols >> initialize: size [

	super initialize: size.
	index := 0
]

{ #category : #accessing }
ASZigSymbols >> virutalName [

	index := index + 1.
	^ 'Sym.i_' , index asString
]
