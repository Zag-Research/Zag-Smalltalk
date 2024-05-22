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

{ #category : 'accessing' }
ASCInlineBlock >> locals: locals arguments: arguments [

	stack isEmpty
		ifTrue: [ self pushParameterNamed: #self ]
		ifFalse: [ stack := stack names: #( self ) ].
			arguments reversed do: [ :each | self pushParameterNamed: each ].
	locals do: [ :each | self pushParameterNamed: each ]
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: label
]
