"
I am a reference to a parent Method/BlockClosure variable.

I wrap that in whatever is needed to access the parent's context.
"
Class {
	#name : #ASCParentsVariable,
	#superclass : #ASCVariable,
	#instVars : [
		'variable'
	],
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCParentsVariable >> variable: aVariable [

	variable := aVariable
]
