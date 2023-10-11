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

{ #category : #compiling }
ASZigCPSOutput class >> blockClass [ ^ ASZigCPSBlockOutput 
]

{ #category : #accessing }
ASZigCPSOutput >> checkBreakAtCall: aSymbol [
	^ true
]

{ #category : #'code generation' }
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

{ #category : #initialization }
ASZigCPSOutput >> initialize [

	super initialize.
	auxStream := '' writeStream
]

{ #category : #'local variable handling' }
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
ASZigCPSOutput >> outputMethodEnd [

	mainStream nextPutAll: '}
'
]

{ #category : #'code generation' }
ASZigCPSOutput >> outputMethodLabel: bbLabel [

	auxStream
		nextPut: $&;
		nextPutAll: bbLabel zigName;
		nextPut: $,.

]

{ #category : #'code generation' }
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

{ #category : #'code generation' }
ASZigCPSOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator unorderedBasicBlocksDo: [ :bb |
		maxDepth := maxDepth max: bb maxDepth.
		bb outputTo: self.
 ]
]

{ #category : #'code generation' }
ASZigCPSOutput >> putTestOpposite: test [

	test = #ifTrue ifTrue: [
		^ self
			nextPutAll: 'if(!';
			  rVal: #sp;
			  nextPutAll: '.top().isFalse())@panic("non-boolean");' ].
	self error
]

{ #category : #'local variable handling' }
ASZigCPSOutput >> rVal: aString [

	self nextPutAll: (lVals at: aString ifAbsent: [ aString ])
]

{ #category : #'local variable handling' }
ASZigCPSOutput >> sp: anInteger [ 
	self rVal: #sp;nextPutAll: '.at(';print: anInteger ;nextPut: $)
]

{ #category : #'code generation' }
ASZigCPSOutput >> verifySelector: aSymbol [

	self
		nextPutAll: 'if(!';
		putDirectSymbol: aSymbol;
		nextPutAll:
			'.hashEquals(selector))return @call(tailCall,dnu,.{pc,sp,process,context,selector,cache})';
		endStatement
]

{ #category : #API }
ASZigCPSOutput >> visitCall: aSelector doBreak: doBreak [

	self halt
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

{ #category : #API }
ASZigCPSOutput >> visitPushBlock: anUndefinedObject [ 
	self nextPutAll: 'ABLOCK'
]

{ #category : #API }
ASZigCPSOutput >> visitPushLiteral: aLiteral [

	self
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.push(';
		nextPutAll: aLiteral asZigLiteral;
		nextPut: $);
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

{ #category : #API }
ASZigCPSOutput >> visitPushSelf: anASCSelf [

	self
		lVal: #sp;
		rVal: #sp;
		nextPutAll: '.push(';
		rVal: #context;
		nextPutAll: '.self())';
		endStatement
]

{ #category : #API }
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
		nextPutAll:
			'return @call(tailCall, cContext.npc, .{ cContext.tpc, ';rVal: #sp;nextPutAll:', process, callerContext, selector, cache })';
		endStatement;
		flush
]

{ #category : #API }
ASZigCPSOutput >> visitSend: aSelector returnIndex: index [

	self
		nextPutAll: 'context.setReturn(&';
		nextPutAll: componentName;
		nextPutAll: '.code[';
		print: index - 1;
		nextPutAll: '])';
		endStatement;
		lConst: #pc;
		nextPutAll: 'dispatch.lookup(';
		putDirectSymbol: aSelector;
		nextPutAll: ', ';
		sp: aSelector numArgs;
		nextPutAll: '.classIndex)';
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

{ #category : #writing }
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

{ #category : #writing }
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
