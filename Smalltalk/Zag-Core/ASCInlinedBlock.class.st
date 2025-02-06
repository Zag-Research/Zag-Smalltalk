"
I am the superclass of Code Units that are inlined, contrasted with ones that are dispatchable.
"
Class {
	#name : 'ASCInlinedBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'API' }
ASCInlinedBlock >> blockClosureClass [

	^ ASCInlineClosureBlock
]

{ #category : 'instance creation' }
ASCInlinedBlock >> newInlinedBlock: aMethodAST [
	^ ASCInlineMethodBlock new
]

{ #category : 'API' }
ASCInlinedBlock >> returnTopFor: aBasicBlock [
"self halt.
^ aBasicBlock returnTop"

]

{ #category : 'as yet unclassified' }
ASCInlinedBlock >> selfElementClass [
	^ self subclassResponsibility
]
