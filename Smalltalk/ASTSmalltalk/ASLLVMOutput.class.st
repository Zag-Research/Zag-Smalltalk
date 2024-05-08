Class {
	#name : 'ASLLVMOutput',
	#superclass : 'ASZigMethodOutput',
	#instVars : [
		'sequence'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'API' }
ASLLVMOutput >> endMethod [
	self label: nil
]

{ #category : 'as yet unclassified' }
ASLLVMOutput >> iniitialize [

	super initialize.
	sequence := 0
]

{ #category : 'as yet unclassified' }
ASLLVMOutput >> mapLLVMTypeFor: type [
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

{ #category : 'as yet unclassified' }
ASLLVMOutput >> nextVariable [

	sequence := sequence + 1.
	^ '%v' , sequence asString
]

{ #category : 'code generation' }
ASLLVMOutput >> outputBasicBlockEnd [

]

{ #category : 'code generation' }
ASLLVMOutput >> outputMethodLabel: bbLabel [

	self
		nextPutAll: ') { ';
		nextPutAll: bbLabel asString;
		nextPutAll: ': '.
]

{ #category : 'code generation' }
ASLLVMOutput >> outputMethodStart: bbLabel [ 

	| type llvmType |	
	"Geting the type of the top element in the stack"
	type := self codeGenerator returnCode stack top valueClass.
	llvmType := self mapLLVMTypeFor: type.
	
	self 
		nextPutAll: 'define ';			
		nextPutAll: llvmType asString.
]

{ #category : 'code generation' }
ASLLVMOutput >> outputToStream [
	"Method yet to be implemented"
	codeGenerator finalAnalysis.
	"Something--"
	codeGenerator basicBlocksUnorderedDo: [ :bb |
		currentBasicBlock := bb.
		currentBasicBlock outputTo: self. ]
]

{ #category : 'code generation' }
ASLLVMOutput >> verifySelector: anObject [

	self 
		nextPutAll: ' @';
		nextPutAll: anObject; 
	"Other args can be added before the '{'"
		nextPutAll: '('.
]

{ #category : 'API' }
ASLLVMOutput >> visitPushContext: unused [

	self
		nextPutAll: '(';
		"Insert LLVM arguments here"nextPutAll: ')'
]

{ #category : 'API' }
ASLLVMOutput >> visitPushLiteral: aLiteral [

	self 
		nextPutAll: (self mapLLVMTypeFor: aLiteral) asString;  
		nextPut: $ ;
		nextPutAll: aLiteral asString.
]

{ #category : 'API' }
ASLLVMOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]
