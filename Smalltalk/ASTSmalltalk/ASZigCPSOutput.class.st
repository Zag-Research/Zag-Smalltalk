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
ASZigCPSOutput >> outputEnd: bb [

	skipRest ifTrue: [ ^ self ].
	bb test = #returnTop ifTrue: [  ].
	bb test ifNotNil: [ :else |
		self
			nextPutAll: '    return @call(tailCall,';
			id: else label;
			nextPutAll: ',.{';
			id: self name;
			nextPutAll: '.at(';
			print: else number + 1;
			nextPutAll: '), sp, process, context, selector, cache });
' ]	.
bb else ifNotNil: [ :else |
		self
			nextPutAll: '    return @call(tailCall,';
			id: else label;
			nextPutAll: ',.{';
			id: self name;
			nextPutAll: '.at(';
			print: else number + 1;
			nextPutAll: '), sp, process, context, selector, cache });
' ]
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

{ #category : #'local variable handling' }
ASZigCPSOutput >> rVal: aString [

	self nextPutAll: (lVals at: aString ifAbsent: [ aString ])
]

{ #category : #API }
ASZigCPSOutput >> startMethod: primitiveNumber [

	primitiveNumber = 0
		ifTrue: [ self putEmbedded: #verifySelector ]
		ifFalse: [ self putPrimitive: primitiveNumber ]
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
ASZigCPSOutput >> visitReturn [

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
		rVal: #sp;
		nextPutAll: '.top=';
		rVal: #top;
		endStatement;
		lConst: #cContext;
		rVal: #result;
		nextPutAll: '.ctxt';
		endStatement;
		nextPutAll:
			'return @call(tailCall, cContext.npc, .{ cContext.tpc, newSp, process, callerContext, selector, cache })';
		endStatement;
		flush
]

{ #category : #API }
ASZigCPSOutput >> visitSend: aSelector doBreak: doBreak [

	self
		nextPutAll: 'context.setReturn(';
		nextPut: $);
		endStatement ;
		lConst: #pc;
		nextPutAll: 'dispatch.lookup(newSelector, .SmallInteger)';
		endStatement ;
		nextPutAll:     'return @call(tailCall,';
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
