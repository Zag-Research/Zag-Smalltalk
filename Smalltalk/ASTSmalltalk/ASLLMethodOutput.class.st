"
I am the putput for methods for LLVM
"
Class {
	#name : 'ASLLMethodOutput',
	#superclass : 'ASLLOutput',
	#instVars : [
		'sequence',
		'stack',
		'inMethod'
	],
	#classVars : [
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
			           (String -> 'i8*') }
]

{ #category : 'initialization' }
ASLLMethodOutput >> initialize [

	super initialize.
	stack := OrderedCollection new
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

	self print: anASCInlineBlock label;
		nextPut: $:;
		cr
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMBlockLabel: bbLabel [

	self
		nextPutAll: bbLabel;
		nextPut: $:;
		lf
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
		nextPutAll: '.CM = global %struct.CompiledMethod { i64 ';
		print: header;
		nextPutAll: ', %union.Object { i64 ';
		print: stackStructure;
		nextPutAll: ' }, %struct.MethodSignature ';
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
	inMethod := true
]

{ #category : 'accessing' }
ASLLMethodOutput >> prefix [

	^ ''
]

{ #category : 'API - remotes' }
ASLLMethodOutput >> push [

	| var |
	var := currentBasicBlock nextSequence.
	self nextPutAll: '  %';print: var; nextPutAll: ' = '.
	stack add: var.
]

{ #category : 'API' }
ASLLMethodOutput >> setupForOutput [

	| bbs |
	inMethod := false.
	bbs := Set new.
	codeGenerator basicBlocksUnorderedDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: bb maxDepth.
		bbs add: bb ].
	[ bbs isEmpty ] whileFalse: [
		| temp |
		temp := bbs.
		bbs := Set new.
		temp do: [ :bb | bb flow ifTrue: [ bbs add: bb ] ] ]
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
ASLLMethodOutput >> visitPushContext: unused [

	self
		nextPutAll: '(';
		"Insert LLVM arguments here"nextPutAll: ')'
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

{ #category : 'API' }
ASLLMethodOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
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
