"
I am an inline block, but I'm used to inline a Method at the top level - i.e. with a possible context
Returns from me are real returns
"
Class {
	#name : 'ASCCompiledMethodBlock',
	#superclass : 'ASCDispatchableBlock',
	#traits : 'ASCMergeBlocks',
	#classTraits : 'ASCMergeBlocks classTrait',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASCCompiledMethodBlock >> blockClosureClass [

	^ ASCTopMethodClosureBlock
]

{ #category : 'adding' }
ASCCompiledMethodBlock >> branchedToFrom: aBasicBlock [
	" nothing to do, because this can only happen with tail recursive calls "

	self halt
]

{ #category : 'adding' }
ASCCompiledMethodBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	^ stack
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
	methodBlock := self.
]

{ #category : 'initialization' }
ASCCompiledMethodBlock >> label [

	^ label
]

{ #category : 'accessing' }
ASCCompiledMethodBlock >> locals: locals parameters: arguments [

	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	stack := sourceElements := ASStackContext fromStack: stack
]

{ #category : 'writing' }
ASCCompiledMethodBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> propagateContextFreeFor [

	^ self propagateContextFreeFor: sourceElements " context is the first thing on the stack"
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> returnTopFor: aBasicBlock [ 

	^ aBasicBlock returnTop
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> selector: aSymbol class: aClass codeUnit: aBasicBlock [

	label := aClass name , '.' , aSymbol
]

{ #category : 'API' }
ASCCompiledMethodBlock >> sendTail: aSymbol targetClass: target for: aBasicBlock [

	^ aBasicBlock sendTail: aSymbol targetClass: target
]

{ #category : 'writing' }
ASCCompiledMethodBlock >> sourceElementsForInspect [

	^ OrderedCollection new addAll: sourceElements replicate;yourself
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> zigWalkReturn: anASReturn for: anASCodeGenerator [ 
	self shouldBeImplemented.
]
