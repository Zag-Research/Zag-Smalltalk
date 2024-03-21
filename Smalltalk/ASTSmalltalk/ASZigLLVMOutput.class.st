Class {
	#name : 'ASZigLLVMOutput',
	#superclass : 'ASZigMethodOutput',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'as yet unclassified' }
ASZigLLVMOutput >> Testing [

	"With another comment - again testing..."
]

{ #category : 'API' }
ASZigLLVMOutput >> endMethod [
	self label: nil
]

{ #category : 'as yet unclassified' }
ASZigLLVMOutput >> mapLLVMTypeFor: anObject [
	"Maps Pharo datatypes to LLVM's" 
   | pharoType llvmType |
    
    pharoType := anObject class.
    
    llvmType := pharoType caseOf: {
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
	"Assuming we always recevie an ASCMethodBlock"
	ascBlock outputRestTo: self. 
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputMethodLabel: bbLabel [

	"Not needed.."
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputMethodStart: bbLabel [ 
	"Not implemented in threaded code generator either"
	self 
		nextPutAll: 'define <type> @'.
		"nextPutAll: bbLabel;"
		"nextPutAll: '() '" 	
]

{ #category : 'code generation' }
ASZigLLVMOutput >> outputToStream [
	"Method yet to be implemented"
	codeGenerator finalAnalysis.
	"Something--"
	codeGenerator basicBlocksUnorderedDo: [ :bb |
		currentBasicBlock := bb.
		self outputLLVMFrom: currentBasicBlock. ]
]

{ #category : 'code generation' }
ASZigLLVMOutput >> verifySelector: anObject [

	"Does this verify the selector?"
	self 
		nextPutAll: anObject.
]

{ #category : 'code generation' }
ASZigLLVMOutput >> visitPushLiteral: aLiteral [

	self 
		nextPutAll: (self mapLLVMTypeFor: aLiteral) asString;  
		nextPut: $ ;
		nextPutAll: aLiteral asString.
]

{ #category : 'code generation' }
ASZigLLVMOutput >> visitReturnTop [

	self 
		nextPutAll: 'ret '.
]
