"
I am a BasicBlock that is the return target for a send or call

Note that I often get changed to be an `ASCJoinBlock`, so my objects must be the same size.
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#classVars : [
		'TargetBlockPrototype'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'data flow instructions' }
ASCReturnBlock >> becomeJoinBlock [

	self primitiveChangeClassTo: ASCJoinBlock basicNew.
	stack makeStackTheSource
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForLocals [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForParameters [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForSelf [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForTemporaries [

	^ ASFlowOnDemand
]

{ #category : 'initialization' }
ASCReturnBlock >> llvmLabel [

	^ '0'
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> namedBlock [

	^ self
]

{ #category : 'successor sequences' }
ASCReturnBlock >> nextSequence [

	sequence ifNil: [ ^ sequence := 1 ].
	^ sequence := sequence + 1
]

{ #category : 'writing' }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCReturnBlock >> selector [

	^self  methodBlock selector
]
