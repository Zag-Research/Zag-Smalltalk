"
I am a BasicBlock that is the return target for a send or call
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'method',
		'sequence'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCReturnBlock class >> for: anASCBlock [ 
	^ super new method: anASCBlock namedMethod
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

	^ label ifNil: [ label := String streamContents: [ :s |
					 s
						 nextPutAll: method selector;
						 nextPut: $_;
						 print: method nextSequence ] ]
]

{ #category : 'accessing' }
ASCReturnBlock >> method: anASCMethodBlock [ 
	method := anASCMethodBlock
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

{ #category : 'accessing' }
ASCReturnBlock >> selector [

	^ method selector
]
