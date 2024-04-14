"
I represent a method in AST Intermediate Represeantation, which parallels the Opal IR
"
Class {
	#name : 'ASTIrMethod',
	#superclass : 'Object',
	#instVars : [
		'primitive',
		'selector',
		'numArgs',
		'tempMap',
		'startBlockNumber',
		'blocks'
	],
	#category : 'ASTSmalltalk-IR',
	#package : 'ASTSmalltalk',
	#tag : 'IR'
}

{ #category : 'initialization' }
ASTIrMethod >> addSequence: anIRSequence [
	self addSequence: anIRSequence offset: 0
]

{ #category : 'initialization' }
ASTIrMethod >> addSequence: anIRSequence offset: offset [ 
	[ anIRSequence orderNumber + offset > blocks size] whileTrue: [blocks add: nil].
	(blocks at: anIRSequence orderNumber) ifNil: [ blocks at: anIRSequence orderNumber put: anIRSequence asAST ]
]

{ #category : 'initialization' }
ASTIrMethod >> from: aCompiledMethod [ 
	| seq |
	aCompiledMethod  compiledMethod primitive = 0 ifFalse: [ primitive := aCompiledMethod  compiledMethod primitive ].
	selector := aCompiledMethod sourceNode selector.
	numArgs := aCompiledMethod numArgs.
	tempMap := aCompiledMethod ir tempMap.
	seq := aCompiledMethod ir startSequence.
	startBlockNumber := seq orderNumber.
	blocks := OrderedCollection new.
	seq withAllSuccessorsDo: [ : irSequence |
		self addSequence: irSequence ]
]
