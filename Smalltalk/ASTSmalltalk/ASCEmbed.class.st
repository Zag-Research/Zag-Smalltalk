"
I represent an embedded send replacement.
"
Class {
	#name : 'ASCEmbed',
	#superclass : 'ASCSend',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCEmbed >> outputTo: anOutput [

	^ anOutput
		  visitEmbed: selector
		  returnIndex: (bb ifNotNil: [ bb index ])
		  target: target
]
