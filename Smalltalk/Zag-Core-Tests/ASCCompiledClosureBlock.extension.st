Extension { #name : 'ASCCompiledClosureBlock' }

{ #category : '*Zag-Core-Tests' }
ASCCompiledClosureBlock >> getMethodFor: aSymbol else: aBlockClosure [

	astBlock selector = aSymbol ifTrue: [ ^ self ].
	^ aBlockClosure value: BlockClosure
]
