"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : 'ASZigThreadedOutput',
	#superclass : 'ASZigMethodOutput',
	#classTraits : 'ASOutputAPI classTrait',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASZigThreadedOutput class >> blockClass [
	^ ASZigThreadedBlockOutput
]

{ #category : 'API' }
ASZigThreadedOutput >> branch: command to: target [
	command = #br ifTrue: [ 
	delayedLabel := target label ] ifFalse: [ self putEmbedded: command. target ifNotNil: [self labelRef: target label ]]
]

{ #category : 'accessing' }
ASZigThreadedOutput >> methodName [
	| predefined |
	predefined := context predefinedSymbol: self selector.
	predefined ifNotNil: [ ^predefined ] ifNil: [ ^ self name]
]

{ #category : 'code generation' }
ASZigThreadedOutput >> outputBasicBlockEnd [
	
]

{ #category : 'code generation' }
ASZigThreadedOutput >> outputMethodDefinition: bbLabel primitive: aPrimitive [
]

{ #category : 'API' }
ASZigThreadedOutput >> outputMethodEnd [

	self label: nil
]

{ #category : 'code generation' }
ASZigThreadedOutput >> outputMethodLabel: bbLabel [

	self label: bbLabel
]

{ #category : 'code generation' }
ASZigThreadedOutput >> outputMethodStart: bbLabel [


]

{ #category : 'API' }
ASZigThreadedOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)';
		lf
]

{ #category : 'writing' }
ASZigThreadedOutput >> primitive: anInteger [

	self
		nextPutAll: '&p.p';
		print: anInteger;
		comma;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma;
		lf
]

{ #category : 'code generation' }
ASZigThreadedOutput >> verifySelector: anObject [

	self putEmbedded: #verifySelector;
	lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitCase: aClassToBlockMap [
	
	self putEmbedded: #case;
	comma;
	lf
]

{ #category : 'visiting' }
ASZigThreadedOutput >> visitEmbed: aSelector [
	self
		putEmbedded: aSelector;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPopLocal: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		context: [ :local |
			self
				putEmbedded: #popLocal;
				print: local;
				comma;
				lf ]
		stack: [ :offset |
			self
				putEmbedded: #popStack;
				print: offset;
				comma;
				lf ]
		other: nil
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushBlock: anASCBlock [

	anASCBlock manifest
		ifTrue: [ 
			self putEmbedded: #BlockClosure, '.', #fullClosure;
			nextPut: $";
			nextPutAll: anASCBlock label; 
			nextPut: $";
			comma; 
			lf]
		ifFalse: [ self visitPushLiteral: nil ]
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushContext [
	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushContext: unused [
	self
		putEmbedded: #pushContext;
		nextPutAll: '"^"';
		comma;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushLocal: anASLocal [

^	self visitPushLocalOrBlock: anASLocal
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushLocalOrBlock: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		context: [ :local |
			self
				putEmbedded: #pushLocal;
				print: local;
				comma;
				lf ]
		stack: [ :offset |
			self
				putEmbedded: #pushStack;
				print: offset;
				comma;
				lf ]
		other: nil
]

{ #category : 'API' }
ASZigThreadedOutput >> visitPushNil [

	self putEmbedded: #pushNil;
		comma;
		lf
]

{ #category : 'visiting' }
ASZigThreadedOutput >> visitReturnNil [

	self putEmbedded: #returnNil;
	lf
]

{ #category : 'visiting' }
ASZigThreadedOutput >> visitReturnSelf [

		self putEmbedded: #returnSelf;
		lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitReturnTop [

	self putEmbedded: #returnTop;
	lf
]

{ #category : 'API' }
ASZigThreadedOutput >> visitSend: aSelector returnIndex: index target: aClass [

	self
		putEmbedded: 'setupSend';
		putSymbol: aSelector;
		comma;
		lf;
		putEmbedded: 'dynamicDispatch';
		lf
]

{ #category : 'visiting' }
ASZigThreadedOutput >> visitStoreLocal: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		context: [ :local |
			self
				putEmbedded: #storeLocal;
				print: local;
				comma;
				lf ]
		stack: [ :offset |
			self
				putEmbedded: #storeStack;
				print: offset;
				comma;
				lf ]
		other: nil
]

{ #category : 'writing' }
ASZigThreadedOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileMethod(';
		nextPutAll: self methodName;
		nextPut: $,;
		print: numLocals; "Daniel: not doing anything with this, should be easy to fix"
		nextPut: $,;
		print: maxDepth;
		nextPut: $,;
		nextPut: $.;
		nextPutAll: forClass name;
		nextPutAll: ',.{';
		lf;
		nextPutAll: mainStream contents;
		nextPutAll: ' });';
		lf
]

{ #category : 'writing' }
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
