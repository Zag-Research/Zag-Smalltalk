"
I am an abstract class for all the compilable unitss that are dispatchable as opposed to inlined.
"
Class {
	#name : 'ASCDispatchableBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'owns',
		'returnBlocks'
	],
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'categories' }
ASCDispatchableBlock class >> recategorizeSelector: selector from: oldProtocol to: newProtocol [ 
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASCDispatchableBlock >> deriveVariableReferences: referenced modifies: modified [

	operations do: [ :op | op variableReferences: referenced modifies: modified block: self ]
]

{ #category : 'as yet unclassified' }
ASCDispatchableBlock >> hasNonLocalReturn [
	^ false
]

{ #category : 'initialization' }
ASCDispatchableBlock >> initialize [

	super initialize.
	owns := IdentitySet new.
	returnBlocks := Dictionary new
]

{ #category : 'accessing' }
ASCDispatchableBlock >> locals: locals parameters: arguments [

	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	stack := sourceElements := ASStackContext fromStack: stack
]

{ #category : 'accessing' }
ASCDispatchableBlock >> methodBlock [
]

{ #category : 'accessing' }
ASCDispatchableBlock >> owns [

	^ owns
]

{ #category : 'accessing' }
ASCDispatchableBlock >> owns: anObject [

	owns := anObject
]
