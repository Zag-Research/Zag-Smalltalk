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

{ #category : 'as yet unclassified' }
ASCInlinedBlock >> selfElementClass [
	^ self subclassResponsibility
]
