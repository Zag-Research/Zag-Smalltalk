"
I'm an inline block that will be the join point for an `ASCCase`.

That mean I have multiple sources, but they all came from the same place, so they do not create loops 
"
Class {
	#name : 'ASCJoinBlock',
	#superclass : 'ASCReturnBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCJoinBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	^ stack
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> flowForLocals [

	^ nil
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> flowForParameters [

	^ nil
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> flowForSelf [

	^ nil
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> flowForTemporaries [

	^ ASFlowPhi
]

{ #category : 'testing' }
ASCJoinBlock >> isNamedBlock [

	^ false
]

{ #category : 'initialization' }
ASCJoinBlock >> label [

	^ label ifNil: [ label := self nextSequence asString ]
]

{ #category : 'initialization' }
ASCJoinBlock >> llvmLabel [

	^ self label
]

{ #category : 'adding' }
ASCJoinBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names offset: 0 forBlock: self
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> namedBlock [

	^ sourceBlocks anyOne namedBlock
]

{ #category : 'successor sequences' }
ASCJoinBlock >> nextSequence [

	^self  methodBlock nextSequence
]

{ #category : 'writing' }
ASCJoinBlock >> outputTo: output [

	output outputInlineBlockStart: self.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCJoinBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'data flow instructions' }
ASCJoinBlock >> selfElementClass [

	^ ASStackSelf
]

{ #category : 'printing' }
ASCJoinBlock >> updateStack: aSymbol [
	^ nil
]
