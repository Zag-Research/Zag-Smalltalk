"
I am an inline block, but I'm used to inline a BlockClosure at the top level - i.e. with a possible context

Returns from me are real non-local returns
My final expression will be a real return
"
Class {
	#name : 'ASCCompiledClosureBlock',
	#superclass : 'ASCClosureBlock',
	#instVars : [
		'returnBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCCompiledClosureBlock >> addBlock: anASCReturnBlock [

	self halt.
	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> flowForLocals [

	^ ASFlowNil
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> flowForTemporaries [

	^ self shouldNotImplement
]

{ #category : 'testing' }
ASCCompiledClosureBlock >> hasContextOnEntry [

	^ sourceElements hasContext
]

{ #category : 'initialization' }
ASCCompiledClosureBlock >> labelExtra: s [

]

{ #category : 'writing' }
ASCCompiledClosureBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		methods: { self } , returnBlocks keys.
	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCCompiledClosureBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'API' }
ASCCompiledClosureBlock >> returnNonLocal [

	^ self addOperation: ASCReturnNonLocal new
]

{ #category : 'accessing' }
ASCCompiledClosureBlock >> stackDepth [

	^ sourceElements offset
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> updateDataDependencies: ignored [
	" a MethodBlock has no dependencies "

]
