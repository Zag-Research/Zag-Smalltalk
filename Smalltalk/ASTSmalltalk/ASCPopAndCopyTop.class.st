"
I drop n elements from below the top of stack
"
Class {
	#name : 'ASCPopAndCopyTop',
	#superclass : 'ASCReturnTop',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCPopAndCopyTop >> outputTo: output [

	^ output visitPopAndCopyTop: self
]
