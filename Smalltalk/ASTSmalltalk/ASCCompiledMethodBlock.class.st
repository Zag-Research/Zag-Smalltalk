"
I am an inline block, but I'm used to inline a Method at the top level - i.e. with a possible context
Returns from me are real returns
"
Class {
	#name : 'ASCCompiledMethodBlock',
	#superclass : 'ASCMethodBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCCompiledMethodBlock >> addSourceBlock: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'API' }
ASCCompiledMethodBlock >> initParameterNamed: aSymbol [

	| source |
	stack := stack initParameterNamed: aSymbol.
	source := ASFlowParameter new.
	^ stack data: source
]

{ #category : 'API' }
ASCCompiledMethodBlock >> initSelf: targetClass [

	| source |
	stack := stack initSelf.
	source := AStackValue fromClass: targetClass.
	^ stack data: source
]

{ #category : 'initialization' }
ASCCompiledMethodBlock >> initialize [

	super initialize.
	returnBlocks := Dictionary new.
	method := self
]

{ #category : 'writing' }
ASCCompiledMethodBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'API' }
ASCCompiledMethodBlock >> returnSelf [

	^ self addOperation: ASCReturnSelf new
]

{ #category : 'accessing' }
ASCCompiledMethodBlock >> sender: sender locals: locals parameters: arguments [

	self initSelf: sender.
	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	sourceElements := stack
]

{ #category : 'writing' }
ASCCompiledMethodBlock >> sourceElementsForInspect [

	^ OrderedCollection new addAll: sourceElements replicate;yourself
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> zigWalkReturn: anASReturn for: anASCodeGenerator [ 
	self shouldBeImplemented.
]
