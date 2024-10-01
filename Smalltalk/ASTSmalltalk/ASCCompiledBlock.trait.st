Trait {
	#name : 'ASCCompiledBlock',
	#instVars : [
		'declares',
		'owns',
		'modifies',
		'references',
		'returnBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCCompiledBlock >> modified [

	^ modifies
]

{ #category : 'accessing' }
ASCCompiledBlock >> modified: anObject [

	modifies := anObject
]

{ #category : 'accessing' }
ASCCompiledBlock >> referenced [

	^ references
]

{ #category : 'accessing' }
ASCCompiledBlock >> referenced: anObject [

	references := anObject
]
