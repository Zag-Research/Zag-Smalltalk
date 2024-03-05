"
I am a reference to a parent Method/BlockClosure variable.

I wrap that in whatever is needed to access the parent's context.
"
Class {
	#name : 'ASCIndirectVariable',
	#superclass : 'ASCVariable',
	#instVars : [
		'variable'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCIndirectVariable >> variable: aVariable [

	variable := aVariable
]
