"
I am an inline block, used to inline blcok closures defined in a ASCInlineMethodBlock.

Returns from me are stack adjustments and a branch to the methods original return point in the containing method.
"
Class {
	#name : 'ASCTopMethodClosureBlock',
	#superclass : 'ASCInlineClosureBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'instance creation' }
ASCTopMethodClosureBlock >> newInlineBlock: aBlockAST [
	^ aBlockAST newInlineBlock: self
]
