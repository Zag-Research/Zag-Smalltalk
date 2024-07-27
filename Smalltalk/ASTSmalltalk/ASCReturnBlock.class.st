"
I am a BasicBlock that is the return target for a send or call

Note that `ASCInlineBlock`s often get their class changed to me, so my objects must be the same size.
Among other things that means that some slots end up in `ASCBasicBlock` rather than their more logical place of here
"
Class {
	#name : 'ASCReturnBlock',
	#superclass : 'ASCBasicBlock',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCReturnBlock >> addSourceBlock: aBasicBlock [

	sourceBlocks add: aBasicBlock andIfNewDo: [
		sourceElements
			ifNil: [
			sourceElements := self copyStackFrom: aBasicBlock ]
			ifNotNil: [
				sourceElements
					with: aBasicBlock stack
					do: [ :stackElement :other | stackElement mergeWith: other ] ] ]
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForLocals [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForParameters [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForSelf [

	^ ASFlowOnDemand
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> flowForTemporaries [

	^ ASFlowOnDemand
]

{ #category : 'testing' }
ASCReturnBlock >> hasContextOnEntry [

	^ true
]

{ #category : 'initialization' }
ASCReturnBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: ($_ join: ($: split: self selector));
				           nextPut: $.;
				           print: (self methodBlock addBlock: self) ] ]
]

{ #category : 'initialization' }
ASCReturnBlock >> llvmLabel [

	^ '0'
]

{ #category : 'data flow instructions' }
ASCReturnBlock >> namedBlock [

	^ self
]

{ #category : 'successor sequences' }
ASCReturnBlock >> nextSequence [

	sequence ifNil: [ ^ sequence := 1 ].
	^ sequence := sequence + 1
]

{ #category : 'writing' }
ASCReturnBlock >> outputTo: output [

	output outputMethodStart: self.
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCReturnBlock >> selector [

	^self  methodBlock selector
]
