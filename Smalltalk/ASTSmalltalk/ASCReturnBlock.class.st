"
I am a BasicBlock that is the return target for a send or call
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'sequence',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCReturnBlock >> addReturnBlock: anASCReturnBlock [

	^ method addReturnBlock: anASCReturnBlock
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> classForLocals [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> classForTemporaries [

	^ ASFlowOnDemand
]

{ #category : 'accessing' }
ASCReturnBlock >> context: anASStackContext [ 

	context := anASStackContext
]

{ #category : 'writing' }
ASCReturnBlock >> inspectionZagCode [

	<inspectorPresentationOrder: 0 title: 'ZagCode'>
	^ SpTreeTablePresenter new
		  roots: operations;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Blocks' evaluated: #contents);
		  yourself
]

{ #category : 'initialization' }
ASCReturnBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: method selector;
				           nextPut: $.;
				           print: (method addReturnBlock: self) ] ]
]

{ #category : 'initialization' }
ASCReturnBlock >> llvmLabel [

	^ '0'
]

{ #category : 'accessing' }
ASCReturnBlock >> method: anASCMethodBlock [

	method := anASCMethodBlock.
	anASCMethodBlock addReturnBlock: self
]

{ #category : 'API' }
ASCReturnBlock >> namedContext: blockLocal stack: blockStack [ 

	stack namedContext: blockLocal stack: blockStack
]

{ #category : 'accessing' }
ASCReturnBlock >> namedMethod [

	^ self
]

{ #category : 'successor sequences' }
ASCReturnBlock >> nextSequence [

	sequence ifNil: [ ^ sequence := 1 ].
	^ sequence := sequence +1
]

{ #category : 'writing' }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: label.
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCReturnBlock >> selector [

	^ selector ifNil: [ selector := method selector]
]
