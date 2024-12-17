Extension { #name : 'ASCBasicBlock' }

{ #category : '*Zag-Core-Tests' }
ASCBasicBlock >> doPush: anObject [

	stack := stack pushDataAsTemporary: (AStackValue fromValue: anObject).
	maxDepth := maxDepth max: (stackDepth := stackDepth + 1)
]

{ #category : '*Zag-Core-Tests' }
ASCBasicBlock >> pushNil [

	stack := stack pushDataAsTemporary: AStUndefinedClass singleton
]
