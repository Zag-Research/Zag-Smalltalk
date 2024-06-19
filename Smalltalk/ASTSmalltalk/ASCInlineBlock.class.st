"
for inlining
"
Class {
	#name : 'ASCInlineBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'sourceBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlineBlock >> addSource: aBasicBlock [

	self namedMethod == aBasicBlock namedMethod ifFalse: [
		| returnBlock |
		returnBlock := ASCReturnBlock for: method.
		returnBlock hash: hash.
		^ self becomeForward: returnBlock ].
	sourceBlocks add: aBasicBlock.
	stack := aBasicBlock stack sourceFor: self
]

{ #category : 'data flow instructions' }
ASCInlineBlock >> classForLocals [

	^ ASFlowPhi
]

{ #category : 'data flow instructions' }
ASCInlineBlock >> classForTemporaries [

	^ ASFlowPhi
]

{ #category : 'initialization' }
ASCInlineBlock >> initialize [

	super initialize.
	sourceBlocks := Set new
]

{ #category : 'initialization' }
ASCInlineBlock >> label [

	^ label ifNil: [ label := method nextSequence asString ]
]

{ #category : 'initialization' }
ASCInlineBlock >> llvmLabel [

	^ self label
]

{ #category : 'printing' }
ASCInlineBlock >> locals: locals arguments: arguments [
	self nameStack: arguments.
	(stack at: #self) source: self.
	locals do: [ :each | self pushLocalNamed: each ].
	maxDepth := 0
]

{ #category : 'accessing' }
ASCInlineBlock >> method: anASCMethodBlock [ 
	method := anASCMethodBlock
]

{ #category : 'adding' }
ASCInlineBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names offset: 0 forBlock: self
]

{ #category : 'data flow instructions' }
ASCInlineBlock >> namedMethod [
	^ method namedMethod
]

{ #category : 'successor sequences' }
ASCInlineBlock >> nextSequence [

	^ method nextSequence
]

{ #category : 'writing' }
ASCInlineBlock >> outputTo: output [

	output outputMethodLabel: self label.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: self label
]

{ #category : 'printing' }
ASCInlineBlock >> returnTop [

	^ self put: ASCReturnTop new
]

{ #category : 'data flow instructions' }
ASCInlineBlock >> updateDependencies [

	| result |
	super updateDependencies.
	result := OrderedCollection new.
	sources do: [ :requirement |
		(sourceBlocks anySatisfy: [ :block | block providesNonNil: requirement ])
			ifTrue: [
				result addAll:
					(sourceBlocks select: [ :block |
						 block reference: requirement ]) ] ].
	^ result
]

{ #category : 'printing' }
ASCInlineBlock >> updateStack: aSymbol [
	^ nil
]
