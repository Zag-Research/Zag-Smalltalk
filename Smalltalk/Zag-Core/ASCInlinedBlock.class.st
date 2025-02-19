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

{ #category : 'API' }
ASCInlinedBlock >> branchReturnTop: aBasicBlock [ 
	self addOperation: (ASCBranchReturnTop with: branchBlock)
]

{ #category : 'instance creation' }
ASCInlinedBlock >> locals: locals parameters: arguments [
	locals do: [ :each | self initLocalNamed: each ]
]

{ #category : 'instance creation' }
ASCInlinedBlock >> newInlineBlockNonLocalReturn [

	^ ASCTopMethodClosureBlock new
]

{ #category : 'instance creation' }
ASCInlinedBlock >> newInlineBlockReturnSelf [

	^ self class new
]

{ #category : 'API' }
ASCInlinedBlock >> returnNonLocal [

	"hasNonLocalReturn := true."
	^ self addOperation: ASCReturnNonLocal new
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
