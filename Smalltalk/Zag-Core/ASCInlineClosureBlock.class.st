"
I am an inline block,  used to inline a BlockClosure

Returns from me are stack adjustment and branch to the return point for the defining method
My final expression will be a stack adjustment and branch to the return point, replacing `self` with `nil`
"
Class {
	#name : 'ASCInlineClosureBlock',
	#superclass : 'ASCInlinedBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'API' }
ASCInlineClosureBlock >> branchReturnTop: aBasicBlock [ 
	self addOperation: (ASCBranchReturnTop with: branchBlock)
]

{ #category : 'instance creation' }
ASCInlineClosureBlock >> locals: locals parameters: arguments [
	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	stack := sourceElements := ASStackContext fromStack: stack
]

{ #category : 'instance creation' }
ASCInlineClosureBlock >> newInlineBlockNonLocalReturn [

	^ ASCTopMethodClosureBlock new
]

{ #category : 'instance creation' }
ASCInlineClosureBlock >> newInlineBlockReturnSelf [

	^ self class new
]

{ #category : 'API' }
ASCInlineClosureBlock >> returnNonLocal [

	^ self addOperation: (ASCBranchReturnNonLocal with: branchBlock)
]

{ #category : 'API' }
ASCInlineClosureBlock >> selfElementClass [

	^ ASStackNamedIndirect
]
