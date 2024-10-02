"
I am an inline block, but I'm used to inline a Method at the top level - i.e. with a possible context
Returns from me are real returns
"
Class {
	#name : 'ASCCompiledMethodBlock',
	#superclass : 'ASCMethodBlock',
	#traits : 'ASCCompiledBlock',
	#classTraits : 'ASCCompiledBlock classTrait',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCCompiledMethodBlock >> branchedToFrom: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> initCompilableUnitBlockFrom: anASMethodNode [

	selector := anASMethodNode selector.
	primitive := anASMethodNode primitive.
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

	stack := stack initSelf.
	^ stack data: (AStackValue fromClass: targetClass)
]

{ #category : 'initialization' }
ASCCompiledMethodBlock >> initialize [
	super initialize.
	returnBlocks := Dictionary new.
	method := self.
	self initializeASCCompiledBlock 
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
ASCCompiledMethodBlock >> selector: aString [

	selector := aString.
	label := aString
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
