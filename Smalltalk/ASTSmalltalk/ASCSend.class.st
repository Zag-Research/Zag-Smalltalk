"
I represent a normal send.

I  need to store return addresses before dispatch.
"
Class {
	#name : 'ASCSend',
	#superclass : 'ASCTailSend',
	#instVars : [
		'blockToReturnTo'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'callgraph' }
ASCSend >> dotEdgesOn: aStream from: aBasicBlock [

	aStream << '  '.
	aBasicBlock dotNameOn: aStream.
	aStream << ':end:e ->'.
	blockToReturnTo dotNameOn: aStream.
	^ aStream << ';
'
]

{ #category : 'callgraph' }
ASCSend >> dotStringOn: aStream [

	aStream
		<< '<end>send ';
		print: selector
]

{ #category : 'printing' }
ASCSend >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector;
		<< ' -> ';
		<< blockToReturnTo label
]

{ #category : 'Collection API' }
ASCSend >> interpretFor: anInterpreter [

	^ anInterpreter returnTo: blockToReturnTo;send: selector
]

{ #category : 'testing' }
ASCSend >> isTailcall [

	^ false
]

{ #category : 'accessing' }
ASCSend >> nextBasicBlocks [

	^ blockToReturnTo
]

{ #category : 'code generation' }
ASCSend >> outputTo: anOutput [

	^ anOutput visitSend: selector returnIndex: blockToReturnTo receiver: target
]

{ #category : 'handling' }
ASCSend >> return: aBasicBlock [

	blockToReturnTo := aBasicBlock 
]
