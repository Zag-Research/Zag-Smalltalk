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

{ #category : 'API' }
ASCInlineMethodBlock >> locals: locals parameters: arguments [ 
	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
]

{ #category : 'adding' }
ASCInlineMethodBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names offset: 0 forBlock: self
]

{ #category : 'adding' }
ASCInlineMethodBlock >> newInlineBlock: aBlockAST [
	^ aBlockAST newInlineBlock: self
]

{ #category : 'adding' }
ASCInlineMethodBlock >> newInlineMethodBlockNonLocalReturn [
	^ ASCTopMethodClosureBlock new
]

{ #category : 'adding' }
ASCInlineMethodBlock >> newInlineMethodBlockReturnSelf [
	^ ASCInlineClosureBlock new
]

{ #category : 'as yet unclassified' }
ASCInlineMethodBlock >> returnBlock: aBasicBlock [

	returnBlock := aBasicBlock
]

{ #category : 'API' }
ASCInlineMethodBlock >> selfElementClass [

	^ ASStackNamedIndirect 
]

{ #category : 'API' }
ASCInlineMethodBlock >> sendTail: aSymbol targetClass: target for: aBasicBlock [ 
	
	^ aBasicBlock sendTail: aSymbol targetClass: target
]
