"
I am an abstract class for all the compilable unitss that are dispatchable as opposed to inlined.
"
Class {
	#name : 'ASCCompiledBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'owns',
		'returnBlocks',
		'astBlock'
	],
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'categories' }
ASCCompiledBlock class >> recategorizeSelector: selector from: oldProtocol to: newProtocol [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCCompiledBlock >> arguments [

	^ astBlock arguments size
]

{ #category : 'initialization' }
ASCCompiledBlock >> astBlock [

	^ astBlock
]

{ #category : 'as yet unclassified' }
ASCCompiledBlock >> deriveVariableReferences: referenced modifies: modified [

	operations do: [ :op | op variableReferences: referenced modifies: modified block: self ]
]

{ #category : 'as yet unclassified' }
ASCCompiledBlock >> hasNonLocalReturn [
	^ false
]

{ #category : 'API' }
ASCCompiledBlock >> initParameterNamed: aSymbol [

	stack := stack initParameterNamed: aSymbol.
	^ stack data: (AStSubClass new dataClass: Object)
]

{ #category : 'initialization' }
ASCCompiledBlock >> initialize [

	super initialize.
	owns := IdentitySet new.
	returnBlocks := Dictionary new
]

{ #category : 'accessing' }
ASCCompiledBlock >> locals [

	^ astBlock locals size
]

{ #category : 'accessing' }
ASCCompiledBlock >> locals: locals parameters: arguments [

	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	stack := sourceElements := ASStackContext fromStack: stack
]

{ #category : 'accessing' }
ASCCompiledBlock >> methodBlock [
]

{ #category : 'accessing' }
ASCCompiledBlock >> newInlineBlockNonLocalReturn [
	^ ASCTopClosureBlock new
]

{ #category : 'accessing' }
ASCCompiledBlock >> newInlineBlockReturnSelf [
	^ ASCInlineClosureBlock new
]

{ #category : 'writing' }
ASCCompiledBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCCompiledBlock >> owns [

	^ owns
]

{ #category : 'accessing' }
ASCCompiledBlock >> owns: anObject [

	owns := anObject
]

{ #category : 'API' }
ASCCompiledBlock >> propagateContextFreeFor [

	^ self propagateContextFreeFor: sourceElements " context is the first thing on the stack"
]

{ #category : 'initialization' }
ASCCompiledBlock >> setBlock: anASMethodNode [ 

	astBlock := anASMethodNode
]

{ #category : 'testing' }
ASCCompiledBlock >> withMethodOrBlock: aBasicBlock [
 ^ astBlock = aBasicBlock astBlock 
]
