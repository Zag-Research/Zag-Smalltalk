"
I am a separately compiled BlockClosure - structurally equivalent to a ASCCompiledMethodBloc - i.e. with a possible context

Returns from me are real non-local returns
My final expression will be a real return
"
Class {
	#name : 'ASCCompiledClosureBlock',
	#superclass : 'ASCCompiledBlock',
	#instVars : [
		'hasNonLocalReturn'
	],
	#category : 'Zag-Core-BasicBlocks',
	#package : 'Zag-Core',
	#tag : 'BasicBlocks'
}

{ #category : 'adding' }
ASCCompiledClosureBlock >> addBlock: anASCReturnBlock [

	self halt.
	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'API' }
ASCCompiledClosureBlock >> blockClosureClass [

	^ ASCTopClosureBlock
]

{ #category : 'compiling' }
ASCCompiledClosureBlock >> dispatchGetMethodFor: aBlock [

	^ self
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> flowForLocals [

	^ ASFlowNil
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> flowForTemporaries [

	^ self shouldNotImplement
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> hasNonLocalReturn [
	^ hasNonLocalReturn
]

{ #category : 'initialization' }
ASCCompiledClosureBlock >> initNamedIndirect: aBasicBlock [

	stack := stack initNamedIndirect myScope: aBasicBlock sourceElements
]

{ #category : 'initialization' }
ASCCompiledClosureBlock >> initialize [

	super initialize.
	hasNonLocalReturn := false
]

{ #category : 'initialization' }
ASCCompiledClosureBlock >> labelExtra: s [

]

{ #category : 'printing' }
ASCCompiledClosureBlock >> printOn: aStream [

	aStream nextPutAll: '>>'; nextPutAll: self label
]

{ #category : 'API' }
ASCCompiledClosureBlock >> returnTopFor: aBasicBlock current: current [

	^ current returnTop
]

{ #category : 'API' }
ASCCompiledClosureBlock >> returnTopNonLocal [
	hasNonLocalReturn := true.
	^ self addOperation: ASCReturnNonLocal new
]

{ #category : 'initialization' }
ASCCompiledClosureBlock >> selector: aSymbol class: aClass generator: aGenerator [

	label := aGenerator blockLabel,'.',aSymbol
]

{ #category : 'accessing' }
ASCCompiledClosureBlock >> stackDepth [

	^ sourceElements offset
]

{ #category : 'data flow instructions' }
ASCCompiledClosureBlock >> updateDataDependencies: ignored [
	" a MethodBlock has no dependencies "

]
