"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCReturnBlock',
	#instVars : [
		'primitive',
		'returnBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addReturnBlock: anASCReturnBlock [

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
ASCMethodBlock >> initSelf: targetClass [

	| source |
	stack := stack pushSelf.
	source := ASFlowDefine new id: #self.
	self addSource: source.
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
	stack requiredContextDo: [ output visitPushContext ].
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
ASCMethodBlock >> selector: aString [

	selector := aString.
	label := aString
]
