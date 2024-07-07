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
ASCInlinedMethodBlock >> className: aString [

	label := self label,'_',aString
]

{ #category : 'printing' }
ASCInlinedMethodBlock >> inspectExtra [

	^ className ifNil: [ '' ]
]
