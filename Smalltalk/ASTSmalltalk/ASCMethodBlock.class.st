"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCReturnBlock',
	#instVars : [
		'primitive',
		'returnBlocks',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addBlock: anASCReturnBlock [

	anASCReturnBlock context: context.
	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'data flow instructions' }
ASCMethodBlock >> classForLocals [

	^ ASFlowNil
]

{ #category : 'data flow instructions' }
ASCMethodBlock >> classForTemporaries [

	^ self shouldNotImplement
]

{ #category : 'accessing' }
ASCMethodBlock >> context [

	^ context
]

{ #category : 'API' }
ASCMethodBlock >> initParameterNamed: aSymbol [

	| source |
	stack := stack initParameterNamed: aSymbol.
	source := ASFlowOnDemand new id: aSymbol.
	^ stack
		  value: (ASCParameter new
				   init: Object;
				   name: aSymbol);
		  source: source
]

{ #category : 'API' }
ASCMethodBlock >> initSelf: targetClass [

	| source |
	stack := stack initSelf.
	source := ASFlowOnDemand new id: #self.
	^ stack
		  value: (ASCSelf new
				   init: targetClass;
				   name: #self);
		  source: source
]

{ #category : 'initialization' }
ASCMethodBlock >> initialize [

	super initialize.
	returnBlocks := Dictionary new.
	method := self
]

{ #category : 'initialization' }
ASCMethodBlock >> labelExtra: s [

]

{ #category : 'accessing' }
ASCMethodBlock >> locals: locals parameters: arguments [

	arguments do: [ :each | self initParameterNamed: each ].
	locals do: [ :each | self initLocalNamed: each ].
	sources := stack.
	maxDepth := 0
]

{ #category : 'accessing' }
ASCMethodBlock >> namedMethod [

	^ self
]

{ #category : 'writing' }
ASCMethodBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks keys.
	output outputMethodStart: label.
	self requiredContextDo: [ output visitPushContext ].
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCMethodBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : 'printing' }
ASCMethodBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'accessing' }
ASCMethodBlock >> selector [

	^ selector
]

{ #category : 'accessing' }
ASCMethodBlock >> selector: aString [

	selector := aString.
	label := aString
]

{ #category : 'data flow instructions' }
ASCMethodBlock >> updateDependencies [
	" a MethodBlock has no dependencies "

	^ #( )
]
