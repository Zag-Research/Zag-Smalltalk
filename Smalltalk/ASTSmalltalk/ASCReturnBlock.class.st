"
I am a BasicBlock that is the return target for a send or call
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCReturnBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: label.
	aStream << $)
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

{ #category : 'writing' }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: label.
	output outputMethodLabel: label.
	^ self outputRestTo: output
]
