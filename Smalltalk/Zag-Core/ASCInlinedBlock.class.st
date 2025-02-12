"
I am the superclass of Code Units that are inlined, contrasted with ones that are dispatchable.
"
Class {
	#name : 'ASCInlinedBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'branchBlock'
	],
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'API' }
ASCInlinedBlock >> blockClosureClass [

	^ ASCInlineClosureBlock
]

{ #category : 'API' }
ASCInlinedBlock >> branch: aBasicBlock [

	branchBlock := aBasicBlock.
	aBasicBlock branchedToFrom: self.
	^ aBasicBlock
]

{ #category : 'instance creation' }
ASCInlinedBlock >> newInlinedBlock: aMethodAST [
	^ ASCInlineMethodBlock new
]

{ #category : 'API' }
ASCInlinedBlock >> returnTopFor: aBasicBlock [ 

	^ aBasicBlock branchReturnTop: branchBlock 
]

{ #category : 'as yet unclassified' }
ASCInlinedBlock >> selfElementClass [
	^ self subclassResponsibility
]
