"
I generate text-based code compiled by the `compileMethod` compile-time Zig function
"
Class {
	#name : #ASZigThreadedOutput,
	#superclass : #ASZigMethodOutput,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #API }
ASZigThreadedOutput >> branch: command to: target [
	command = #br ifTrue: [ 
	delayedLabel := target label ] ifFalse: [ self putEmbedded: command. target ifNotNil: [self labelRef: target label ]]
]

{ #category : #API }
ASZigThreadedOutput >> drop [

	self putEmbedded: #drop
]

{ #category : #API }
ASZigThreadedOutput >> endMethod [

	self label: nil
]

{ #category : #'code generation' }
ASZigThreadedOutput >> outputMethodEnd [
	
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
	codeGenerator linearizeBasicBlocksDo: [ :bb |
		maxDepth := maxDepth max: bb maxDepth.
		bb outputTo: self ].
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
ASZigThreadedOutput >> verifySelector [

	self putEmbedded: #verifySelector
]

{ #category : #API }
ASZigThreadedOutput >> visitPopLocal: localNumber [

	self
		putEmbedded: #popLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> visitPushBlock: stackIndex [

	self tab;
	print: stackIndex;lf
]

{ #category : #API }
ASZigThreadedOutput >> visitPushContext [

	self putEmbedded: #pushContext;
	nextPutAll: '"^"';comma
]

{ #category : #API }
ASZigThreadedOutput >> visitPushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> visitPushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigThreadedOutput >> visitReturnTop [

	self putEmbedded: #returnTop
]

{ #category : #API }
ASZigThreadedOutput >> visitSend: aSelector returnIndex: index [


	self putEmbedded: ('send' , aSelector numArgs asString) asSymbol.
	self putSymbol: aSelector
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
