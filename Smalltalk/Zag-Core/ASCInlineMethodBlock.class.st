"
I am an inline block,  used to inline a Method

Returns from me are stack adjustment and branch to the return point for the defining method
My final expression will be a stack adjustment and branch to the return point, leaving `self` unchanged.
"
Class {
	#name : 'ASCInlineMethodBlock',
	#superclass : 'ASCInlinedBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'adding' }
ASCInlineMethodBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names offset: 0 forBlock: self
]

{ #category : 'as yet unclassified' }
ASCInlineMethodBlock >> returnBlock: aBasicBlock [

	returnBlock := aBasicBlock
]

{ #category : 'API' }
ASCInlineMethodBlock >> selfElementClass [

	^ ASStackSelf
]
