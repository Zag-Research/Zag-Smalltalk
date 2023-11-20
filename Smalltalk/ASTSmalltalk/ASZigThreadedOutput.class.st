"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : #ASZigThreadedOutput,
	#superclass : #ASZigMethodOutput,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #compiling }
ASZigThreadedOutput class >> blockClass [
	^ ASZigThreadedBlockOutput
]

{ #category : #API }
ASZigThreadedOutput >> branch: command to: target [
	command = #br ifTrue: [ 
	delayedLabel := target label ] ifFalse: [ self putEmbedded: command. target ifNotNil: [self labelRef: target label ]]
]

{ #category : #API }
ASZigThreadedOutput >> endMethod [

	self label: nil
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputBasicBlockEnd [
	
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputMethodLabel: bbLabel [

	self label: bbLabel
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputMethodStart: bbLabel [


]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator basicBlocksLinearizeDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: currentBasicBlock maxDepth.
		currentBasicBlock outputTo: self ].
	self endMethod
]

{ #category : #API }
ASZigThreadedOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #writing }
ASZigThreadedOutput >> primitive: anInteger [

	self
		nextPutAll: '&p.p';
		print: anInteger;
		comma 
]

{ #category : #API }
ASZigThreadedOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : #'code generation' }
ASZigThreadedOutput >> verifySelector: anObject [

	self putEmbedded: #verifySelector
]

{ #category : #API }
ASZigThreadedOutput >> visitPopLocal: anASLocal [

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

{ #category : #API }
ASZigThreadedOutput >> visitPushBlock: anASCBlock [

	anASCBlock manifest
		ifTrue: [ self visitPushLocalOrBlock: anASCBlock ]
		ifFalse: [ self visitPushLiteral: nil ]
]

{ #category : #API }
ASZigThreadedOutput >> visitPushContext: anUndefinedObject [

	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma
]

{ #category : #API }
ASZigThreadedOutput >> visitPushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> visitPushLocal: anASLocal [

^	self visitPushLocalOrBlock: anASLocal
]

{ #category : #API }
ASZigThreadedOutput >> visitPushLocalOrBlock: anASLocal [

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

{ #category : #visiting }
ASZigThreadedOutput >> visitReturnNil [

	self putEmbedded: #returnNil
]

{ #category : #visiting }
ASZigThreadedOutput >> visitReturnSelf [

		self putEmbedded: #returnSelf
]

{ #category : #API }
ASZigThreadedOutput >> visitReturnTop [

	self putEmbedded: #returnTop
]

{ #category : #API }
ASZigThreadedOutput >> visitSend: aSelector returnIndex: index target: aClass [

	self
		putEmbedded: ('send' , aSelector numArgs asString
			 , (index ifNil: [ 'tail' ] ifNotNil: [ '' ])) asSymbol;
		putSymbol: aSelector;
		comma
]

{ #category : #visiting }
ASZigThreadedOutput >> visitSimple: aSymbol [ 
	^ self putEmbedded: aSymbol
]

{ #category : #visiting }
ASZigThreadedOutput >> visitStoreLocal: anASLocal [

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

{ #category : #writing }
ASZigThreadedOutput >> writeDefinitionOn: stream [

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

{ #category : #writing }
ASZigThreadedOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
