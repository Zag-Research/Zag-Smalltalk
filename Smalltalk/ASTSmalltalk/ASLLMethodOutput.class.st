Class {
	#name : 'ASLLMethodOutput',
	#superclass : 'ASLLOutput',
	#instVars : [
		'sequence',
		'stack'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASLLMethodOutput class >> blockClass [
	^ ASLLBlockOutput
]

{ #category : 'API' }
ASLLMethodOutput >> endMethod [
	self label: nil
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> initialize [

	super initialize.
	sequence := 0.
	stack := OrderedCollection new
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> mapLLVMTypeFor: type [
	"Maps Pharo datatypes to LLVMs - need to change to dispatch soon" 
   | llvmType |
    
    llvmType := type caseOf: {
        [SmallInteger] -> ['i64'].
        [Float] -> ['double'].
        [Boolean] -> ['i1'].
        [String] -> ['i8*'].
    } otherwise: ['void'].
    
    ^ llvmType
]

{ #category : 'compiling' }
ASLLMethodOutput >> methodGenerator [
	^ self
]

{ #category : 'as yet unclassified' }
ASLLMethodOutput >> nextVariable [

	sequence := sequence + 1.
	^ '%v' , sequence asString
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputBasicBlockEnd [

]

{ #category : 'as yet unclassified' }
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

	| header stackStructure signature verifier methods seperator |
	header := 0.
	stackStructure := 0.
	signature := 0.
	verifier := '@verifyMethod'.
	methods := #(  ).
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
			nextPutAll: each;
			nextPutAll: ' }'.
		seperator := ', ' ].
	self
		nextPutAll: ']}, align 8
'.
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodStart: bbLabel [

	self
		nextPutAll: 'define ptr @';
		nextPutAll: bbLabel;
		nextPutAll:
			'(ptr noundef %pc, ptr noundef %sp, ptr noundef %process, ptr noundef %context, i64 %signature) #1 {
'
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
ASLLMethodOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]

{ #category : 'writing' }
ASLLMethodOutput >> writeDefinitionOn: aWriteStream [

	aWriteStream nextPutAll: mainStream contents
]

{ #category : 'writing' }
ASLLMethodOutput >> writeInitializationOn: aWriteStream [ 

]
