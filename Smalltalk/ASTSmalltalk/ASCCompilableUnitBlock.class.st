"
I have subclasses that are the first block created when compiling a method or block closure.

I can be the target of tail-recuursive sends which means I can have multiple sources which cause loops in the control flow graph
"
Class {
	#name : 'ASCCompilableUnitBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASCCompilableUnitBlock >> blockClosureClass [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> locals: locals parameters: arguments [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnEmpty [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnSelfFor: aBasicBlock [

	^ self subclassResponsibility
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
