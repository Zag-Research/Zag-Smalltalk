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

{ #category : 'initialization' }
ASCInlinedBlock >> initNamedIndirect: aBasicBlock [

	stack := stack initNamedIndirect myScope: aBasicBlock sourceElements
]

{ #category : 'adding' }
ASCInlinedBlock >> initParameterNamed: aSymbol [

	stack := stack initParameterNamed: aSymbol.
	^ stack data: (AStSubClass new dataClass: Object)
]

{ #category : 'API' }
ASCInlinedBlock >> returnTopFor: aBasicBlock [ 

	^ aBasicBlock branchReturnTop: branchBlock 
]

{ #category : 'initialization' }
ASCInlinedBlock >> selector: aString class: aClass generator: anASCodeGenerator [ 
	
	label := aClass name,'.',aString
]

{ #category : 'as yet unclassified' }
ASCInlinedBlock >> selfElementClass [
	^ self subclassResponsibility
]
