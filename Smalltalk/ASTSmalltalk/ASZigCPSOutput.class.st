"
I generate text-based as a sequence of Continuation-Passing-Style Zig function
"
Class {
	#name : #ASZigCPSOutput,
	#superclass : #ASZigMethodOutput,
	#instVars : [
		'auxStream',
		'skipRest',
		'currentLVal',
		'lVals'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #printing }
ASZigCPSOutput class >> testing [
	^ self new testing
]

{ #category : #accessing }
ASZigCPSOutput >> checkBreakAtCall: aSymbol [
	^ true
]

{ #category : #API }
ASZigCPSOutput >> drop [

	self putEmbedded: #drop
]

{ #category : #'local variable handling' }
ASZigCPSOutput >> endStatement [

	mainStream nextPutAll: ';
'.
	currentLVal ifNotNil: [
		lVals at: currentLVal put: currentLVal , '_'.
		currentLVal := nil ]
]

{ #category : #flagging }
ASZigCPSOutput >> flush [
	skipRest := true
]

{ #category : #'local variable handling' }
ASZigCPSOutput >> lVal: aString [

	| s |
	s := lVals at: aString ifAbsent: [
		     self nextPutAll: 'var '.
		     currentLVal := aString.
		aString,'_' ].
	self
		nextPutAll: s;
		nextPut: $=
]

{ #category : #'code generation' }
ASZigCPSOutput >> outputEnd: bb [
	skipRest ifTrue: [ ^ self ].
		bb else ifNotNil: [ :else |
			self
				nextPutAll: '    return @call(tailCall,';
				id: else label;
				nextPutAll: ',.{';
				id: self name;
				nextPutAll: '.at(';
				print: else number + 1;
				nextPutAll: '), sp, process, context, selector, cache });
' ].

]

{ #category : #'code generation' }
ASZigCPSOutput >> outputToStream [

	codeGenerator cleanupAndGetPrimitive ifNotNil: [ :primitive |
		self error ].
	codeGenerator unorderedBasicBlocksDo: [ :bb |
		self
			nextPutAll: 'fn ';
			id: bb label;
			nextPutAll:
				'(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
'.
		skipRest := false.
		currentLVal := nil.
		lVals := Dictionary new.
		bb visitAll: self.
		self outputEnd: bb.
		mainStream nextPutAll: '}    
' ]
]

{ #category : #API }
ASZigCPSOutput >> popLocal: localNumber [

	self
		putEmbedded: #popLocal;
		print: localNumber;
		comma
]

{ #category : #API }
ASZigCPSOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self
		putEmbedded: #popLocalData;
		print: stackIndex;
		nextPutAll: ' + (';
		print: blockIndex;
		nextPutAll: ' << 8), // ';
		nextPutAll: name;
		nextPutAll: ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #API }
ASZigCPSOutput >> pushBlock: stackIndex [

	self tab;
	print: stackIndex;lf
]

{ #category : #API }
ASZigCPSOutput >> pushContext [

	self putEmbedded: #pushContext;
	nextPutAll: '"^"';comma
]

{ #category : #API }
ASZigCPSOutput >> pushLiteral: aLiteral [

	self
		putEmbedded: #pushLiteral;
		nextPutAll: aLiteral asZigLiteral;
		comma
]

{ #category : #API }
ASZigCPSOutput >> pushLocal: localNumber [

	self
		putEmbedded: #pushLocal;
		print: localNumber;
		comma
]

{ #category : #'local variable handling' }
ASZigCPSOutput >> rVal: aString [

	self nextPutAll: (lVals at: aString ifAbsent: [ aString ])
]

{ #category : #API }
ASZigCPSOutput >> returnTop [

	self
		putEmbedded: #returnTop
]

{ #category : #API }
ASZigCPSOutput >> send: aSelector useCall: useCall [

	self putEmbedded: (useCall
			 ifTrue: [ 'call' ]
			 ifFalse: [ 'send' ]) , aSelector numArgs asString.
	self
		nextPutAll: aSelector;
		comma
]

{ #category : #API }
ASZigCPSOutput >> startMethod: primitiveNumber [

	primitiveNumber = 0
		ifTrue: [ self putEmbedded: #verifySelector ]
		ifFalse: [ self putPrimitive: primitiveNumber ]
]

{ #category : #visiting }
ASZigCPSOutput >> visitLiteral: literal [

	self
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.push(';
		nextPutAll: literal asZigLiteral;
		nextPut: $);
		endStatement 
]

{ #category : #visiting }
ASZigCPSOutput >> visitPopLocal: anInteger [ 
	self nextPutAll: 'context.setLocal(';
	print: anInteger;
	nextPutAll: ',';
	rVal: #sp;
	nextPutAll: '.top)';
	endStatement;
	lVal: #sp;
	rVal: #sp;
	nextPutAll: '.drop()';
	endStatement 
]

{ #category : #visiting }
ASZigCPSOutput >> visitPushLocal: anInteger [ 
	self lVal: #sp;
	rVal: #sp;
	nextPutAll: '.push(context.getLocal(';
	print: anInteger;
	nextPutAll: '))';
	endStatement 
]

{ #category : #visiting }
ASZigCPSOutput >> visitSend: aSymbol useCall: aBoolean [ 
	self nextPutAll: 'send:';
	putDirectSymbol: aSymbol;
	flush
]

{ #category : #writing }
ASZigCPSOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: stream.
	stream
		nextPutAll: 'compileMethod(';
		nextPutAll: self name zigName;
		comma;
		print: context numLocals;
		comma;
		print: context maxDepth;
		nextPutAll: '+';
		print: closureSize;
		nextPutAll: ',.{';
		nextPutAll: auxStream contents;
		nextPutAll: ' });';
		lf;
		nextPutAll: mainStream contents 
]

{ #category : #writing }
ASZigCPSOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
