"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCPublicClosureBlock',
	#superclass : 'ASCClosureBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCPublicClosureBlock >> addBlock: anASCReturnBlock [

	self halt.
	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'data flow instructions' }
ASCPublicClosureBlock >> flowForLocals [

	^ ASFlowNil
]

{ #category : 'data flow instructions' }
ASCPublicClosureBlock >> flowForTemporaries [

	^ self shouldNotImplement
]

{ #category : 'testing' }
ASCPublicClosureBlock >> hasContextOnEntry [

	^ sourceElements hasContext
]

{ #category : 'initialization' }
ASCPublicClosureBlock >> labelExtra: s [

]

{ #category : 'writing' }
ASCPublicClosureBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCPublicClosureBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'accessing' }
ASCPublicClosureBlock >> stackDepth [

	^ sourceElements offset
]

{ #category : 'data flow instructions' }
ASCPublicClosureBlock >> updateDataDependencies: ignored [
	" a MethodBlock has no dependencies "

]
