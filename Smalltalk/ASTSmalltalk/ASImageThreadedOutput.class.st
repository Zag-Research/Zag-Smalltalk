"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : 'ASImageThreadedOutput',
	#superclass : 'ASImageOutput',
	#classTraits : 'ASOutputAPI classTrait',
	#instVars : [
		'delayedLabel'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASImageThreadedOutput class >> blockClass [
	^ ASZigThreadedBlockOutput
]

{ #category : 'API' }
ASImageThreadedOutput >> branch: command to: target [

	command = #br
		ifTrue: [ delayedLabel := target label ]
		ifFalse: [
			self putEmbedded: command.
			target ifNotNil: [ self labelRef: target label ] ]
]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputBasicBlockEnd [
	
]

{ #category : 'API' }
ASImageThreadedOutput >> outputMethodEnd [


]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputMethodLabel: bbLabel [


]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputMethodStart: bb [

	
]

{ #category : 'API' }
ASImageThreadedOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : 'writing' }
ASImageThreadedOutput >> primitive: anInteger [

	self
		nextPutAll: '&p.p';
		print: anInteger;
		comma 
]

{ #category : 'API' }
ASImageThreadedOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : 'code generation' }
ASImageThreadedOutput >> verifySelector: anObject [

	^ image putRawInt: ZfVerifySelector
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPopLocal: anASCLocal [

	currentBasicBlock
		offsetOf: anASCLocal name
		context: [ :local |
			self
				putEmbedded: #popLocal;
				print: local;
				comma ]
		stack: [ :offset |
			self
				putEmbedded: #popStack;
				print: offset;
				comma ]
		other: nil
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushBlock: anASCBlock [

	anASCBlock manifest
		ifTrue: [ self visitPushLocalOrBlock: anASCBlock ]
		ifFalse: [ self visitPushLiteral: nil ]
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushContext [

	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushContext: anUndefinedObject [

	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLiteral: aLiteral [

	image
		putRawInt: ZfPushLiteral;
		addOperation: aLiteral
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLocal: anASCLocal [

	^ self visitPushLocalOrBlock: anASCLocal
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLocalOrBlock: anASCLocal [

	currentBasicBlock
		offsetOf: anASCLocal name
		context: [ :local |
			self
				putEmbedded: #pushLocal;
				print: local;
				comma ]
		stack: [ :offset |
			self
				putEmbedded: #pushStack;
				print: offset;
				comma ]
		other: nil
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushNil [

	self putEmbedded: #pushNil;
		comma
]

{ #category : 'visiting' }
ASImageThreadedOutput >> visitReturnNil [

	self putEmbedded: #returnNil
]

{ #category : 'visiting' }
ASImageThreadedOutput >> visitReturnSelf [

	^ image putRawInt: ZfReturnSelf
]

{ #category : 'API' }
ASImageThreadedOutput >> visitReturnTop [

	^ image putRawInt: ZfReturnTop
]

{ #category : 'API' }
ASImageThreadedOutput >> visitSend: aSelector returnIndex: index target: aClass [

	self
		putEmbedded: ('send' , aSelector numArgs asString
			 , (index ifNil: [ 'tail' ] ifNotNil: [ '' ])) asSymbol;
		putSymbol: aSelector;
		comma
]

{ #category : 'visiting' }
ASImageThreadedOutput >> visitSimple: aString [ 
	^ image putRawInt: (Addresses at: aString)
]

{ #category : 'visiting' }
ASImageThreadedOutput >> visitStoreLocal: anASCLocal [

	currentBasicBlock
		offsetOf: anASCLocal name
		context: [ :local |
			self
				putEmbedded: #storeLocal;
				print: local;
				comma ]
		stack: [ :offset |
			self
				putEmbedded: #storeStack;
				print: offset;
				comma ]
		other: nil
]

{ #category : 'writing' }
ASImageThreadedOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileMethod(';
		nextPutAll: self name zigName;
		nextPut: $,;
		print: numLocals;
		nextPut: $,;
		print: maxDepth;
		nextPutAll: ',.{';
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : 'writing' }
ASImageThreadedOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
