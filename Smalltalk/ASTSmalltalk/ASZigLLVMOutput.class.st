Class {
	#name : 'ASZigLLVMOutput',
	#superclass : 'ASZigMethodOutput',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'API' }
ASZigLLVMOutput >> endMethod [
	self label: nil
]

{ #category : 'as yet unclassified' }
ASZigLLVMOutput >> mapLLVMTypeFor: type [
	"Maps Pharo datatypes to LLVMs - need to change to dispatch" 
   | llvmType |
    
    llvmType := type caseOf: {
        [SmallInteger] -> ['i64'].
        [Float] -> ['double'].
        [Boolean] -> ['i1'].
        [String] -> ['i8*'].
    } otherwise: ['void'].
    
    ^ llvmType
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputBasicBlockEnd [
	mainStream nextPutAll: '}'
]

{ #category : 'as yet unclassified' }
ASZigLLVMOutput >> outputLLVMFrom: ascBlock [ 
	ascBlock outputRestTo: self. 
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputMethodLabel: bbLabel [

	self
		nextPutAll: ') { ';
		nextPutAll: bbLabel asString;
		nextPutAll: ': '.
		
	
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputMethodStart: bbLabel [ 

	| type llvmType |	
	"Geting the type of the top element in the stack"
	type := self codeGenerator returnCode stack top valueClass.
	llvmType := self mapLLVMTypeFor: type.
	
	self 
		nextPutAll: 'define ';			
		nextPutAll: llvmType asString.
		
		
		
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputToStream [
	"Method yet to be implemented"
	codeGenerator finalAnalysis.
	"Something--"
	codeGenerator basicBlocksUnorderedDo: [ :bb |
		currentBasicBlock := bb.
		currentBasicBlock outputTo: self. ]
	
]

{ #category : 'code generation' }
ASZigLLVMOutput >> verifySelector: anObject [

	self 
		nextPutAll: ' @';
		nextPutAll: anObject; 
	"Other args can be added before the '{'"
		nextPutAll: '('.
]

{ #category : 'API' }
ASZigLLVMOutput >> visitPushContext: aBlock [ 
	
	self 
		nextPutAll: '(';
		"Insert LLVM arguments here"
		nextPutAll: ')'. 
	

]

{ #category : 'API' }
ASZigLLVMOutput >> visitPushLiteral: aLiteral [

	self 
		nextPutAll: (self mapLLVMTypeFor: aLiteral) asString;  
		nextPut: $ ;
		nextPutAll: aLiteral asString.
		
]

{ #category : 'API' }
ASZigLLVMOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]
