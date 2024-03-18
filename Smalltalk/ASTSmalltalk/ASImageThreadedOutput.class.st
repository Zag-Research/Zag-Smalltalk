"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : 'ASImageThreadedOutput',
	#superclass : 'ASImageOutput',
	#classTraits : 'ASOutputAPI classTrait',
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
	command = #br ifTrue: [ 
	delayedLabel := target label ] ifFalse: [ self putEmbedded: command. target ifNotNil: [self labelRef: target label ]]
]

{ #category : 'API' }
ASImageThreadedOutput >> endMethod [

	self label: nil
]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputBasicBlockEnd [
	
]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputMethodLabel: bbLabel [

	self label: bbLabel
]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputMethodStart: bbLabel [


]

{ #category : 'code generation' }
ASImageThreadedOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator basicBlocksLinearizeDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: currentBasicBlock maxDepth.
		currentBasicBlock outputTo: self ].
	self endMethod
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

	self putEmbedded: #verifySelector
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPopLocal: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		local: [ :local |
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
ASImageThreadedOutput >> visitPushContext: anUndefinedObject [

	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLocal: anASLocal [

^	self visitPushLocalOrBlock: anASLocal
]

{ #category : 'API' }
ASImageThreadedOutput >> visitPushLocalOrBlock: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		local: [ :local |
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

		self putEmbedded: #returnSelf
]

{ #category : 'API' }
ASImageThreadedOutput >> visitReturnTop [

	self putEmbedded: #returnTop
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
ASImageThreadedOutput >> visitStoreLocal: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		local: [ :local |
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
