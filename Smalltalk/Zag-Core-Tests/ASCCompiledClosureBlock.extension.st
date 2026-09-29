Extension { #name : 'ASCCompiledClosureBlock' }

{ #category : '*Zag-Core-Tests' }
ASCCompiledClosureBlock >> getMethodFor: aSymbol else: aBlockClosure [
self halt.
	astBlock selector = aSymbol ifTrue: [ ^ self ].
	^ aBlockClosure value: BlockClosure
]
