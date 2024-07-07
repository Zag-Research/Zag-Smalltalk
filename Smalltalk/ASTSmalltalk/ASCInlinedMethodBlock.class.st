"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCInlinedMethodBlock',
	#superclass : 'ASCInlineBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCInlinedMethodBlock >> method: aCompiledMethod [

	label := String streamContents: [ :s| s  nextPutAll: label; nextPut: $_;print: aCompiledMethod ]
]
