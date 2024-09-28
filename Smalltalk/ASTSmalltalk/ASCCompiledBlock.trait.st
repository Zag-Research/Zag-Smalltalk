Trait {
	#name : 'ASCCompiledBlock',
	#instVars : [
		'declares',
		'owns',
		'modifies',
		'references'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCCompiledBlock >> declares [

	^ declares
]

{ #category : 'accessing' }
ASCCompiledBlock >> declares: anObject [

	declares := anObject
]

{ #category : 'accessing' }
ASCCompiledBlock >> modified [

	^ modifies
]

{ #category : 'accessing' }
ASCCompiledBlock >> modified: anObject [

	modifies := anObject
]

{ #category : 'accessing' }
ASCCompiledBlock >> owns [

	^ owns
]

{ #category : 'accessing' }
ASCCompiledBlock >> owns: anObject [

	owns := anObject
]

{ #category : 'accessing' }
ASCCompiledBlock >> referenced [

	^ references
]

{ #category : 'accessing' }
ASCCompiledBlock >> referenced: anObject [

	references := anObject
]
