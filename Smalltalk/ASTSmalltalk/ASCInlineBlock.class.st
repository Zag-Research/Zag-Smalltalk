"
for inlining
"
Class {
	#name : 'ASCInlineBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlineBlock >> addSourceBlock: aBasicBlock [

	sourceBlocks add: aBasicBlock.
	sourceBlocks size = 1 ifTrue: [
		sources := stack := aBasicBlock copyOfStackFor: self ]
]

{ #category : 'data flow instructions' }
ASCInlineBlock >> checkForCalls [

	(sourceBlocks anySatisfy: #endsWithCall) ifTrue: [
		self primitiveChangeClassTo: ASCReturnBlock basicNew ]
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

	^ label ifNil: [ label := self method nextSequence asString ]
]

{ #category : 'initialization' }
ASCInlineBlock >> llvmLabel [

	^ self label
]

{ #category : 'printing' }
ASCInlineBlock >> locals: locals parameters: arguments [

	self nameStack: arguments.
	locals do: [ :each | self initLocalNamed: each ].
	maxDepth := 0
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

	^ self method namedMethod
]

{ #category : 'successor sequences' }
ASCInlineBlock >> nextSequence [

	^self  method nextSequence
]

{ #category : 'writing' }
ASCInlineBlock >> outputTo: output [

	output outputMethodLabel: self label.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'printing' }
ASCInlineBlock >> returnTop [

	^ self put: ASCReturnTop new
]

{ #category : 'printing' }
ASCInlineBlock >> updateStack: aSymbol [
	^ nil
]
