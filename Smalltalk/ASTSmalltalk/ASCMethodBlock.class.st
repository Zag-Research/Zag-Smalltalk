Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addSourceBlock: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'API' }
ASCMethodBlock >> initParameterNamed: aSymbol [

	| source |
	stack := stack initParameterNamed: aSymbol.
	source := ASFlowParameter new.
	^ stack data: source
]

{ #category : 'API' }
ASCMethodBlock >> initSelf: targetClass [

	| source |
	stack := stack initSelf.
	source := AStackValue fromClass: targetClass.
	^ stack data: source
]

{ #category : 'accessing' }
ASCMethodBlock >> locals: locals parameters: arguments [

	arguments do: [ :each | self initParameterNamed: each ].
"	locals do: [ :each | self initLocalNamed: each ]."
	sourceElements := stack.
	self branch: 42
]

{ #category : 'writing' }
ASCMethodBlock >> sourceElementsForInspect [

	^ OrderedCollection new addAll: sourceElements replicate;yourself
]
