"
I have subclasses that are the first block created when compiling a method or block closure.

I can be the target of tail-recuursive sends which means I can have multiple sources which cause loops in the control flow graph
"
Class {
	#name : 'ASCCompilableUnitBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'API' }
ASCCompilableUnitBlock >> blockClosureClass [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> locals: locals parameters: arguments [

	^ self subclassResponsibility
]

{ #category : 'instance creation' }
ASCCompilableUnitBlock >> newInlinedBlock: aBlockAST [
	"What are we trying to inline? 
		- send that assigns to a variable?
		- send that returns value?
	"
	^ ASCInlineMethodBlock new
	"^ code newInlineMethodBlock: aBlockAST "
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnEmpty [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnSelfFor: aBasicBlock [

	^ aBasicBlock returnSelf
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnTopFor: aBasicBlock [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> selector [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> sendTail: aSymbol targetClass: aTargetClass for: aBasicBlock [

	^ self subclassResponsibility
]
