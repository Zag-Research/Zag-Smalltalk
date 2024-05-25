"
for inlining
"
Class {
	#name : 'ASCInlineBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'initialization' }
ASCInlineBlock >> label [

	^ label ifNil: [ label := method nextSequence asString ]
]

{ #category : 'as yet unclassified' }
ASCInlineBlock >> namedMethod [
	^ method namedMethod
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: self label
]
