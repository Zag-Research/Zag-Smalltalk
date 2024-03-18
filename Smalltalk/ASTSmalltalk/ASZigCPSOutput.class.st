"
I generate text-based as a sequence of Continuation-Passing-Style Zig function
"
Class {
	#name : 'ASZigCPSOutput',
	#superclass : 'ASZigMethodOutput',
	#classTraits : 'ASOutputAPI classTrait',
	#instVars : [
		'auxStream',
		'skipRest',
		'currentLVal',
		'lVals'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASZigCPSOutput class >> blockClass [ ^ ASZigCPSBlockOutput 
]

{ #category : 'accessing' }
ASZigCPSOutput >> checkBreakAtCall: aSymbol [
	^ true
]

{ #category : 'code generation' }
ASZigCPSOutput >> doBasicBlock: bb [

	self
		nextPutAll: '    return @call(tailCall,';
		id: bb label;
		nextPutAll: ',.{';
		id: self name;
		nextPutAll: '.at(';
		print: bb number + 1;
		nextPutAll: '), ';
		rVal: #sp;
		nextPutAll: ', process, context, selector, cache })';
		endStatement 
]

{ #category : 'local variable handling' }
ASZigCPSOutput >> endStatement [

	mainStream nextPutAll: ';
'.
	currentLVal ifNotNil: [
		lVals at: currentLVal put: currentLVal , '_'.
		currentLVal := nil ]
]

{ #category : 'flagging' }
ASZigCPSOutput >> flush [
	skipRest := true
]

{ #category : 'initialization' }
ASZigCPSOutput >> initialize [

	super initialize.
	auxStream := '' writeStream
]

{ #category : 'local variable handling' }
ASZigCPSOutput >> lConst: aString [

	| s |
	s := lVals at: aString ifAbsent: [
		     self nextPutAll: 'const '.
		     currentLVal := aString.
		     aString , '_' ].
	self
		nextPutAll: s;
		nextPut: $=
]

{ #category : 'local variable handling' }
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

{ #category : 'code generation' }
ASZigCPSOutput >> outputBasicBlockEnd [

	mainStream nextPutAll: '}
'
]

{ #category : 'code generation' }
ASZigCPSOutput >> outputMethodLabel: bbLabel [

	auxStream
		nextPut: $&;
		nextPutAll: bbLabel zigName;
		nextPut: $,.

]

{ #category : 'code generation' }
ASZigCPSOutput >> outputMethodStart: bbLabel [


	self
		nextPutAll: 'fn ';
		id: bbLabel;
		nextPutAll:
			'(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
'.
	currentLVal := nil.
	lVals := Dictionary new
]

{ #category : 'code generation' }
ASZigCPSOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator basicBlocksUnorderedDo: [ :bb |
		currentBasicBlock := bb.
		currentBasicBlock outputTo: self ]
]

{ #category : 'code generation' }
ASZigCPSOutput >> putTestOpposite: test [

	test = #ifTrue ifTrue: [
		^ self
			nextPutAll: 'if(!';
			  rVal: #sp;
			  nextPutAll: '.top().isFalse())@panic("non-boolean");' ].
	self error
]

{ #category : 'local variable handling' }
ASZigCPSOutput >> rVal: aString [

	self nextPutAll: (lVals at: aString ifAbsent: [ aString ])
]

{ #category : 'local variable handling' }
ASZigCPSOutput >> sp: anInteger [ 
	self rVal: #sp;nextPutAll: '.at(';print: anInteger ;nextPut: $)
]

{ #category : 'code generation' }
ASZigCPSOutput >> verifySelector: aSymbol [

	self
		nextPutAll: 'if(!';
		putDirectSymbol: aSymbol;
		nextPutAll:
			'.hashEquals(selector))return @call(tailCall,dnu,.{pc,sp,process,context,selector,cache})';
		endStatement
]

{ #category : 'API' }
ASZigCPSOutput >> visitCall: aSelector doBreak: doBreak [

	self halt
]

{ #category : 'API' }
ASZigCPSOutput >> visitPopLocal: anASLocal [

	self
	visitStoreLocal: anASLocal;
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.drop()';
		endStatement
]

{ #category : 'API' }
ASZigCPSOutput >> visitPushBlock: anASCBlock [

	anASCBlock manifest
		ifTrue: [ self visitPushLocalOrBlock: anASCBlock ]
		ifFalse: [ self visitPushLiteral: nil ] 
]

{ #category : 'API' }
ASZigCPSOutput >> visitPushContext: aMethodBlock [

	self
		lVal: #context;
		rVal: #context;
		nextPutAll: '.push(';
		rVal: #sp;
		nextPutAll: ', process, &';
		nextPutAll: componentName;
		nextPutAll: ', 0, 2, 0)';
		endStatement;
		lVal: #sp;
		rVal: #context;
		nextPutAll: '.asNewSp()';
		endStatement
]

{ #category : 'API' }
ASZigCPSOutput >> visitPushLiteral: aLiteral [

	self
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.push(';
		nextPutAll: aLiteral asZigLiteral;
		nextPut: $);
		endStatement 
]

{ #category : 'API' }
ASZigCPSOutput >> visitPushLocal: anASLocal [
^
	self
		visitPushLocalOrBlock: anASLocal name
]

{ #category : 'API' }
ASZigCPSOutput >> visitPushLocalOrBlock: anObject [

	self
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.push('.
	currentBasicBlock
		offsetOf: anObject
		local: [ :local |
			self
				nextPutAll: 'context.getLocal(';
				print: local;
				nextPut: $) ]
		stack: [ :offset |
			self
				rVal: #sp;
				nextPutAll: '.at(';
				print: offset;
				nextPut: $) ]
		other: nil.
	self
		nextPut: $);
		endStatement
]

{ #category : 'API' }
ASZigCPSOutput >> visitReturnTop [

	self
		lConst: #top;
		rVal: #sp;
		nextPutAll: '.top';
		endStatement;
		lConst: #result;
		nextPutAll: 'context.pop(process)';
		endStatement;
		lVal: #sp;
		rVal: #result;
		nextPutAll: '.sp';
		endStatement;
		rVal: #sp;
		nextPutAll: '.top=';
		rVal: #top;
		endStatement;
		lConst: #cContext;
		rVal: #result;
		nextPutAll: '.ctxt';
		endStatement;
		nextPutAll: 'return @call(tailCall, cContext.npc, .{ cContext.tpc, ';
		rVal: #sp;
		nextPutAll: ', process, cContext, selector, cache })';
		endStatement;
		flush
]

{ #category : 'API' }
ASZigCPSOutput >> visitSend: aSelector returnIndex: index target: target [

	index ifNotNil: [self
		nextPutAll: 'context.setReturn(&';
		nextPutAll: componentName;
		nextPutAll: '.code[';
		print: index - 1;
		nextPutAll: '])';
		endStatement].
		self 
		lConst: #pc;
		nextPutAll: 'dispatch.lookup(';
		putDirectSymbol: aSelector;
		nextPutAll: ', '.
	target
		ifNotNil: [
			self
				nextPut: $.;
				nextPutAll: target name;
				nextPut: $) ]
		ifNil: [
			self
				sp: aSelector numArgs;
				nextPutAll: '.classIndex)' ].
	self
		endStatement;
		nextPutAll: 'return @call(tailCall,';
		rVal: #pc;
		nextPutAll: '.prim,.{';
		rVal: #pc;
		nextPutAll: '.next(),';
		rVal: #sp;
		nextPutAll: ', process, context, ';
		putDirectSymbol: aSelector;
		nextPutAll: ', cache });
';
		flush
]

{ #category : 'visiting' }
ASZigCPSOutput >> visitStoreLocal: anASLocal [

	currentBasicBlock
		offsetOf: anASLocal name
		local: [ :local |
			self
				nextPutAll: 'context.setLocal(';
				print: local ]
		stack: [ :offset |
			self
				rVal: #sp;
				nextPutAll: '.atPut(';
				print: offset ]
		other: nil.
	self
		nextPutAll: ',';
		rVal: #sp;
		nextPutAll: '.top)';
		endStatement
]

{ #category : 'writing' }
ASZigCPSOutput >> writeDefinitionOn: stream [

	context writeNewDeclarationFor: self on: self.
	self
		nextPutAll: 'compileMethod(';
		putSymbol: codeGenerator selector;
		nextPut: $,;
		print: numLocals;
		nextPut: $,;
		print: maxDepth;
		nextPutAll: ',.{';
		nextPutAll: auxStream contents;
		nextPutAll: ' });';
		lf.
	stream nextPutAll: mainStream contents
]

{ #category : 'writing' }
ASZigCPSOutput >> writeInitializationOn: stream [

	stream
		nextPutAll: componentName zigName;
		nextPutAll: '.setLiterals(&[_]Object{';
		nextPutAll: self setLiteralString;
		nextPutAll: '},&[_]Object{';
		nextPutAll: self refListString;
		nextPutAll: '},null);';
		lf
]
