Class {
	#name : 'ASLLMethodOutput',
	#superclass : 'ASLLOutput',
	#instVars : [
		'sequence'
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
ASLLMethodOutput >> iniitialize [

	super initialize.
	sequence := 0
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

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodLabel: bbLabel [

	self
		nextPutAll: ') { ';
		nextPutAll: bbLabel asString;
		nextPutAll: ': '.
]

{ #category : 'code generation' }
ASLLMethodOutput >> outputMethodStart: bbLabel [ 

	| type llvmType |	
	"Geting the type of the top element in the stack"
	type := self codeGenerator returnCode stack top valueClass.
	llvmType := self mapLLVMTypeFor: type.
	
	self 
		nextPutAll: 'define ';			
		nextPutAll: llvmType asString.
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
		nextPutAll: (self mapLLVMTypeFor: aLiteral) asString;  
		nextPut: $ ;
		nextPutAll: aLiteral asString.
]

{ #category : 'API' }
ASLLMethodOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]

{ #category : 'writing' }
ASLLMethodOutput >> writeDefinitionOn: aWriteStream [ 

]

{ #category : 'writing' }
ASLLMethodOutput >> writeInitializationOn: aWriteStream [ 

]
