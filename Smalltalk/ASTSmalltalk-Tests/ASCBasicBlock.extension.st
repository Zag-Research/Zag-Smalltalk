Extension { #name : 'ASCBasicBlock' }

{ #category : '*ASTSmalltalk-Tests' }
ASCBasicBlock >> doPush: anObject [

	stack := stack pushDataAsTemporary: (AStackValue fromValue: anObject).
	maxDepth := maxDepth max: (stackDepth := stackDepth + 1)
]

{ #category : '*ASTSmalltalk-Tests' }
ASCBasicBlock >> pushNil [

	stack := stack pushDataAsTemporary: AStUndefinedClass singleton
]
