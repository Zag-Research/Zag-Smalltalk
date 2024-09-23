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
ASCCompilableUnitBlock >> expressionFinal [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> expressionReturn [

	^ self subclassResponsibility
]

{ #category : 'examples' }
ASCCompilableUnitBlock >> returnLiteral: literal [

	^ self addOperation: (ASCReturnLiteral literal: literal)
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnSelf [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASCCompilableUnitBlock >> returnTop [

	^ self addOperation: (ASCReturnTop new)
]

{ #category : 'accessing' }
ASCCompilableUnitBlock >> selector [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCCompilableUnitBlock >> sender: sender locals: locals parameters: arguments [

	^ self subclassResponsibility
]
