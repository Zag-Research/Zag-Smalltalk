"
I am the putput for methods for LLVM
"
Class {
	#name : 'ASLLMethodOutput',
	#superclass : 'ASLLOutput',
	#instVars : [
		'inMethod',
		'currentStackName',
		'currentPcName',
		'currentContextName',
		'currentStackOffset'
	],
	#classVars : [
		'NilName',
		'TypeMap'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASLLMethodOutput class >> blockClass [
	^ ASLLBlockOutput
]

{ #category : 'class initialization' }
ASLLMethodOutput class >> initialize [

	TypeMap := Dictionary newFrom: {
			           (SmallInteger -> 'i64').
			           (Float -> 'double').
			           (Boolean -> 'i1').
			           (String -> 'i8*') }.
	NilName := '%nil'
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> adjustStackTo: anInteger [

	currentStackOffset := anInteger + currentStackOffset.
	^ currentStackName := self offset: currentStackName by: anInteger + currentStackOffset
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> generateTemp [

	^ String streamContents: [ :s |
		  s
			  nextPut: $%;
			  print: codeGenerator generateTemp ]
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> generateTemp: prefix [

	^ String streamContents: [ :s |
		  s
			  nextPut: $%;
			  nextPutAll: prefix;
			  nextPut: $.;
			  print: codeGenerator generateTemp ]
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> load: target offset: offset [

	| name newTarget |
	newTarget := self offset: target by: offset.
	name := self generateTemp.
	self
		nextPutAll: name;
		nextPutAll: ' = load i64, ptr ';
		nextPutAll: newTarget;
		nextPutAll: ', align 8';
		cr.
	^ name
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> loadNPC [

	| temp result |
	temp := self generateTemp.
	result := self generateTemp.
	self
		nextPutAll: temp;
		nextPutAll: ' = getelementptr inbounds @zag.context.Context, ptr ';
		nextPutAll: currentContextName;
		nextPutAll: ', i64 0, i32 3';
		cr;
		nextPutAll: result;
		nextPutAll: ' = load ptr, ptr ';
		nextPutAll: temp;
		nextPutAll: ', align 8';
		cr.
	^ result
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> loadTPC [

	| temp result |
	temp := self generateTemp.
	result := self generateTemp.
	self
		nextPutAll: temp;
		nextPutAll: ' = getelementptr inbounds @zag.context.Context, ptr ';
		nextPutAll: currentContextName;
		nextPutAll: ', i64 0, i32 2';
		cr;
		nextPutAll: result;
		nextPutAll: ' = load i64, ptr ';
		nextPutAll: temp;
		nextPutAll: ', align 8';
		cr.
	^ result
]

{ #category : 'code generation' }
ASLLMethodOutput >> mapLLVMTypeFor: type [
	"Maps Pharo datatypes to LLVMs - need to change to dispatch soon"

	^ TypeMap at: type ifAbsent: [ 'void' ]
]

{ #category : 'compiling' }
ASLLMethodOutput >> methodGenerator [
	^ self
]

{ #category : 'accessing' }
ASLLMethodOutput >> nextVariable [

	^ currentBasicBlock nextSequence asString
]

{ #category : 'code generation' }
ASLLMethodOutput >> nextVariableEqualsOffset: offset from: register named: name [

	| label |
	label := self nextVariable.
	self
		nextPutAll: '  %';
		nextPutAll: label;
		nextPutAll: ' = getelementptr { [0 x i64] }, ptr %';
		nextPutAll: register;
		nextPutAll: ', i64 0, i32 ';
		print: offset;
		nextPutAll: '
  %';
		nextPutAll: name;
		nextPutAll: ' = load i64, ptr %';
		nextPutAll: label;
		nextPutAll: ', align 8
'
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> offset: name by: anInteger [

	| newName |
	newName := (name at: 2) = $s
		           ifTrue: [ self generateTemp: 'sp' ]
		           ifFalse: [ self generateTemp ].
	self
		nextPutAll: newName;
		nextPutAll: ' =  getelementptr inbounds @zag.execute.Stack, ptr ';
		nextPutAll: name;
		nextPutAll: ', i64 0, i32 ';
		print: anInteger;
		cr.
	^ newName
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputAddressFromContext: offset named: name [

	self
		nextPutAll: '  %';
		nextPutAll: name;
		nextPutAll: '.address = getelementptr { [0 x i64] }, ptr %context, i64 0, i32 ';
		print: offset + 7;
		nextPutAll: ''

]

{ #category : 'code generation' }
ASLLMethodOutput >> outputBasicBlockEnd [

]

{ #category : 'code generation' }
ASLLMethodOutput >> outputDefineFromContext: idx named: name [

	^ self nextVariableEqualsOffset: idx + 7 from: 'context' named: name
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputDefineFromStack: idx named: name [

	^ self nextVariableEqualsOffset: idx from: 'sp' named: name
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> outputInlineBlockStart: anASCInlineBlock [

	currentStackName := '%sp'.
	currentStackOffset := anASCInlineBlock methodBlock stackDepth.
	self
		nextPutAll: anASCInlineBlock label;
		nextPut: $:;
		cr
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodDefinition: bbLabel primitive: aPrimitive methods: methods [

	| header stackStructure signature verifier seperator |
	header := 0.
	stackStructure := 0.
	signature := 0.
	verifier := aPrimitive ifNil: [ 'verifyMethod' ].
	seperator := ''.
	self
		nextPutAll: '@';
		nextPutAll: bbLabel;
		nextPutAll: '.CM = global @zag.execute.CompiledMethod { i64 ';
		print: header;
		nextPutAll: ', %union.Object { i64 ';
		print: stackStructure;
		nextPutAll: ' }, @zag.execute.MethodSignature ';
		print: signature;
		nextPutAll: ', ptr ';
		nextPutAll: verifier;
		nextPutAll: ', [';
		print: methods size;
		nextPutAll: ' x %union.Code] ['.
	methods do: [ :each |
		self
			nextPutAll: seperator;
			nextPutAll: '%union.Code { ptr @';
			nextPutAll: each label;
			nextPutAll: ' }'.
		seperator := ', ' ].
	self nextPutAll: ']}, align 8
'
]

{ #category : 'API' }
ASLLMethodOutput >> outputMethodEnd [

	inMethod ifFalse: [ ^ self ].
	self nextPutAll: '}
'.
	inMethod := false
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodLabel: bbLabel [

]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodStart: bb [

	self outputMethodEnd.
	self
		nextPutAll: 'define ptr @';
		nextPutAll: bb label;
		nextPutAll:
			'(ptr noundef %pc, ptr noundef %sp, ptr noundef %process, ptr noundef %context, i64 %signature) #1 {
'.
	inMethod := true.
	currentStackName := '%sp'.
	currentStackOffset := bb methodBlock stackDepth.
	currentPcName := '%pc'.
	currentContextName := '%context'
]

{ #category : 'accessing' }
ASLLMethodOutput >> prefix [

	^ ''
]

{ #category : 'API - remotes' }
ASLLMethodOutput >> push [

	| var |
	var := currentBasicBlock nextSequence.
	self
		nextPutAll: '  %';
		print: var;
		nextPutAll: ' = '.
self halt
]

{ #category : 'visiting' }
ASLLMethodOutput >> return: anOperation do: aBlock [

	| theSelf tpc npc |
	theSelf := anOperation theSelf.
	theSelf isInContext ifTrue: [ self halt ].
	self adjustStackTo: theSelf offset.
	aBlock value.
	tpc := self loadTPC.
	npc := self loadNPC.
	self tailcall: npc tpc: tpc signature: nil
]

{ #category : 'API' }
ASLLMethodOutput >> setupForOutput [

	inMethod := false.

]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> store: source atPointer: target [

	self nextPutAll: '  store i64 ';nextPutAll: source; nextPutAll: ', ptr ';nextPutAll: target;nextPutAll: ', align 8';cr
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> store: source atPointer: target offset: anInteger [

	^ self store: source atPointer: (self offset: target by: anInteger)
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> tailcall: npc tpc: tpc signature: signature [

	| temp |
	temp := self generateTemp.
	self
		nextPutAll: temp;
		nextPutAll: ' = musttail call ptr ';
		nextPutAll: npc;
		nextPutAll: '(i64 ';
		nextPutAll: tpc;
		nextPutAll: ', ptr nonnull align 8 ';
		nextPutAll: currentStackName;
		nextPutAll: ', ptr nonnull align 8 %process, ptr nonnull align 8 ';
		nextPutAll: currentContextName;
		nextPutAll: ', i64 ';
		nextPutAll: (signature ifNil: [ 'undef' ]);
		nextPutAll: ')
  ret ptr ';
		nextPutAll: temp;
		cr
]

{ #category : 'code generation' }
ASLLMethodOutput >> verifySelector: anObject [

	self 
		nextPutAll: ' @';
		nextPutAll: anObject; 
	"Other args can be added before the '{'"
		nextPutAll: '('.
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitBranch: anASCInlineBlock [ 

	^ self nextPutAll: '  br label %';
		nextPutAll: anASCInlineBlock label;
		nextPutAll: '
'
]

{ #category : 'API' }
ASLLMethodOutput >> visitCase: aClassToBlockMap [


	^ self nextPutAll: 'visitCase';print: aClassToBlockMap;nextPutAll: '
'
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitDrop [
	self nextPutAll: 'visitDrop'; cr
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitEmbed: aString [ 

	self nextPutAll: 'visitEmbed: ';nextPutAll: aString;nextPutAll: '
'
]

{ #category : 'API' }
ASLLMethodOutput >> visitPushBlock: aBlockClosure [

	self
		nextPutAll: 'pushblock';
		print: aBlockClosure;
		cr
]

{ #category : 'API' }
ASLLMethodOutput >> visitPushContext [

]

{ #category : 'API' }
ASLLMethodOutput >> visitPushLiteral: aLiteral [

	self
		push;
		nextPutAll: (self mapLLVMTypeFor: aLiteral class) asString;
		nextPut: $ ;
		nextPutAll: aLiteral asString;
		lf
]

{ #category : 'API' }
ASLLMethodOutput >> visitPushLocal: anASCLocal [

	self nextPutAll: 'visitPushLocal:
'
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitPushNil [
	self nextPutAll: 'visitPushNil';cr
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnNil: anASCReturnTop [

	^ self
		  return: anASCReturnTop
		  do: [ self store: NilName atPointer: currentStackName ]
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnSelf: anASCReturnSelf [

	^ self return: anASCReturnSelf do: [ ]
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnTop: anASCReturnTop [

	| name |
	name := anASCReturnTop getTop: self.
	^ self return: anASCReturnTop do: [self store: name atPointer: currentStackName ]
]

{ #category : 'API' }
ASLLMethodOutput >> visitSend: aSelector returnIndex: index target: aClass [

	^ self
		  nextPutAll: 'visitSend';
		  print: aSelector;
		  nextPutAll: '
'
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitSimple: aString [

	self nextPutAll: 'visitSimple: ';nextPutAll: aString;nextPutAll: '
'
]

{ #category : 'writing' }
ASLLMethodOutput >> writeDefinitionOn: aWriteStream [

	aWriteStream nextPutAll: mainStream contents
]

{ #category : 'writing' }
ASLLMethodOutput >> writeInitializationOn: aWriteStream [ 

]
