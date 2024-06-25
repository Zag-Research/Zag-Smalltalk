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

{ #category : 'data flow instructions' }
ASCReturnBlock >> checkForCalls [

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

{ #category : 'initialization' }
ASCReturnBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: ($_ join: ($: split: self selector));
				           nextPut: $.;
				           print: (self method addBlock: self) ] ]
]

{ #category : 'initialization' }
ASCReturnBlock >> llvmLabel [

	^ '0'
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

	^self  method selector
]
