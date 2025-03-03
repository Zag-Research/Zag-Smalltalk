"
I am an inline block, but I'm used to inline a BlockClosure defined in an `ASCCompiledMethodBlock`

Returns from me are real returns
My final result will be a stack adjustment and branch to the return point, replacing `self` with `nil`
"
Class {
	#name : 'ASCTopClosureBlock',
	#superclass : 'ASCInlineClosureBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'instance creation' }
ASCTopClosureBlock >> newInlineBlock: aBlockAST [
	^ aBlockAST newInlineTopClosureBlock: self
]

{ #category : 'instance creation' }
ASCTopClosureBlock >> returnTopNonLocal [
	"self alert: 'this is less than what the original does, 	hasNonLocalReturn := true.'."
	^ self addOperation: ASCReturnNonLocal new
]
