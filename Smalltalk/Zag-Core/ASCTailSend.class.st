"
I respresent tail calls, which require some stack manipuation before doing the dispatch
"
Class {
	#name : 'ASCTailSend',
	#superclass : 'ASCSends',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#instVars : [
		'restructure',
		'selfLocation'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'callgraph' }
ASCTailSend >> dotEdgesOn: aWriteStream from: aBasicBlock [
]

{ #category : 'callgraph' }
ASCTailSend >> dotStringOn: aStream [

	aStream
		<< 'tail send ';
		print: selector
]

{ #category : 'printing' }
ASCTailSend >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector;
		<< ' ( tailcall )'
]

{ #category : 'testing' }
ASCTailSend >> isTailcall [

	^ true
]

{ #category : 'accessing' }
ASCTailSend >> nextBasicBlocks [

	^ nil
]

{ #category : 'code generation' }
ASCTailSend >> outputTo: anOutput [

	^ anOutput visitTailSend: selector receiver: target
]

{ #category : 'stack' }
ASCTailSend >> resultStack: anASStack [

	| nArgs |
	nArgs := selector numArgs + 1.
	selfLocation := anASStack at: #self.
	restructure := 0.
	1 to: nArgs do: [ :n |
		restructure := restructure bitOr: (nArgs + 1 - n bitShift: 8 * n) ].
	^ super resultStack: anASStack
]
