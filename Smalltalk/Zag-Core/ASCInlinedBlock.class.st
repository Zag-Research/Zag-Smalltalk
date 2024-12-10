"
I am the superclass of Code Units that are inlined, contrasted with ones that are dispatchable.
"
Class {
	#name : 'ASCInlinedBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASCInlinedBlock >> blockClosureClass [

	^ ASCInlineClosureBlock
]

{ #category : 'as yet unclassified' }
ASCInlinedBlock >> selfElementClass [
	^ self subclassResponsibility
]
