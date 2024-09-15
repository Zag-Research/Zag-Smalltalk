Class {
	#name : 'ASCPublicMethodBlock',
	#superclass : 'ASCMethodBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCPublicMethodBlock >> addSourceBlock: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'API' }
ASCPublicMethodBlock >> initParameterNamed: aSymbol [

	| source |
	stack := stack initParameterNamed: aSymbol.
	source := ASFlowParameter new.
	^ stack data: source
]

{ #category : 'API' }
ASCPublicMethodBlock >> initSelf: targetClass [

	| source |
	stack := stack initSelf.
	source := AStackValue fromClass: targetClass.
	^ stack data: source
]

{ #category : 'accessing' }
ASCPublicMethodBlock >> sender: sender locals: locals parameters: arguments [

	self initSelf: sender.
	arguments do: [ :each | self initParameterNamed: each ].
	"	locals do: [ :each | self initLocalNamed: each ]."
	sourceElements := stack.
	self branch: 42
]

{ #category : 'writing' }
ASCPublicMethodBlock >> sourceElementsForInspect [

	^ OrderedCollection new addAll: sourceElements replicate;yourself
]
