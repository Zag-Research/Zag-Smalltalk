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
ASCCompiledBlock >> declares [

	^ declares
]

{ #category : 'accessing' }
ASCCompiledBlock >> declares: anObject [

	declares := anObject
]

{ #category : 'analyzing' }
ASCCompiledBlock >> deriveOwnership [
	owns addAll: (declares intersection: references).
	owns addAll: (declares intersection: modifies ).
]

{ #category : 'accessing' }
ASCCompiledBlock >> initializeASCCompiledBlock [
	references := Set new.
	modifies := Set new.
	owns := Set new.
	declares := Set new.
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
