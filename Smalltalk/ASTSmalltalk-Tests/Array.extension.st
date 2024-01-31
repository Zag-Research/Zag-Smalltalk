Extension { #name : 'Array' }

{ #category : '*ASTSmalltalk-Tests' }
Array >> doP1 [
	Smalltalk garbageCollect.
	^ (([ self do: [: arg | arg p1 ] ] timeToRun asNanoSeconds * 100) + (self size//2)) // self size
]
