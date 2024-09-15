"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCBlockClosureBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCBlockClosureBlock >> addBlock: anASCReturnBlock [

	self halt.
	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'data flow instructions' }
ASCBlockClosureBlock >> flowForLocals [

	^ ASFlowNil
]

{ #category : 'data flow instructions' }
ASCBlockClosureBlock >> flowForTemporaries [

	^ self shouldNotImplement
]

{ #category : 'testing' }
ASCBlockClosureBlock >> hasContextOnEntry [

	^ sourceElements hasContext
]

{ #category : 'initialization' }
ASCBlockClosureBlock >> labelExtra: s [

]

{ #category : 'writing' }
ASCBlockClosureBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCBlockClosureBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'accessing' }
ASCBlockClosureBlock >> stackDepth [

	^ sourceElements offset
]

{ #category : 'data flow instructions' }
ASCBlockClosureBlock >> updateDataDependencies: ignored [
	" a MethodBlock has no dependencies "

]
