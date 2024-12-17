"
I am an inline block, but I'm used to inline a Method at the top level - i.e. with a possible context
Returns from me are real returns
"
Class {
	#name : 'ASCCompiledMethodBlock',
	#superclass : 'ASCDispatchableBlock',
	#instVars : [
		'primitive'
	],
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
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

{ #category : 'accessing' }
ASCCompiledMethodBlock >> codeSize [

	^ operations size
]

{ #category : 'adding' }
ASCCompiledMethodBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	^ stack
]

{ #category : 'API' }
ASCCompiledMethodBlock >> initParameterNamed: aSymbol [

	stack := stack initParameterNamed: aSymbol.
	^ stack data: (AStSubClass new dataClass: Object)
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

{ #category : 'accessing' }
ASCCompiledMethodBlock >> selector: aString [ 
	self shouldBeImplemented.
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> selector: aSymbol class: aClass codeUnit: aBasicBlock [

	label := aClass name , '.' , aSymbol
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> selector: aSymbol class: aClass generator: aGenerator [

	label := aClass name , '.' , aSymbol
]

{ #category : 'API' }
ASCCompiledMethodBlock >> sendTail: aSymbol targetClass: target for: aBasicBlock [

	^ aBasicBlock sendTail: aSymbol targetClass: target
]

{ #category : 'as yet unclassified' }
ASCCompiledMethodBlock >> zigWalkReturn: anASReturn for: anASCodeGenerator [ 
	self shouldBeImplemented.
]
