"
I am a code generator for blocks... very minor but important differences from methods
"
Class {
	#name : 'ASCodeGeneratorBlock',
	#superclass : 'ASCodeGenerator',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASCodeGeneratorBlock >> returnSelf [
self error: 'need to push self'.
	^ code returnTopNonlocal
]
