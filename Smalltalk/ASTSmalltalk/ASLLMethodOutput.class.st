Class {
	#name : 'ASLLMethodOutput',
	#superclass : 'ASLLOutput',
	#instVars : [
		'sequence',
		'stack'
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

{ #category : 'API' }
ASLLMethodOutput >> endMethod [
	self label: nil
]

{ #category : 'initialization' }
ASLLMethodOutput >> initialize [

	super initialize.
	sequence := 0.
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

	sequence := sequence + 1.
	^ sequence asString
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputBasicBlockEnd [

]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMBlockLabel: bbLabel [

	self
		nextPut: $@;
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

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodLabel: bbLabel [

]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodStart: bbLabel [

	sequence := 0.
	self
		nextPutAll: 'define ptr @';
		nextPutAll: bbLabel;
		nextPutAll:
			'(ptr noundef %pc, ptr noundef %sp, ptr noundef %process, ptr noundef %context, i64 %signature) #1 {
'.
	currentBasicBlock inspect.
	self halt
]

{ #category : 'accessing' }
ASLLMethodOutput >> prefix [

	^ 'L'
]

{ #category : 'API - remotes' }
ASLLMethodOutput >> push [

	| var |
	var := currentBasicBlock nextSequence.
	self nextPutAll: '  %';print: var; nextPutAll: ' = '.
	stack add: var.
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
	self shouldBeImplemented.
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitEmbed: aString [ 
	self shouldBeImplemented.
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
		nextPutAll: (self mapLLVMTypeFor: aLiteral) asString;  
		nextPut: $ ;
		nextPutAll: aLiteral asString;
		lf.
]

{ #category : 'API' }
ASLLMethodOutput >> visitPushLocal: anASLocal [

	self shouldBeImplemented 
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]

{ #category : 'visiting' }
ASLLMethodOutput >> visitSimple: aString [ 
	self shouldBeImplemented.
]

{ #category : 'writing' }
ASLLMethodOutput >> writeDefinitionOn: aWriteStream [

	aWriteStream nextPutAll: mainStream contents
]

{ #category : 'writing' }
ASLLMethodOutput >> writeInitializationOn: aWriteStream [ 

]
