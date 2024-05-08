"
I output BlockClosures
"
Class {
	#name : 'ASLLVMBlockOutput',
	#superclass : 'ASLLVMOutput',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'accessing' }
ASLLVMBlockOutput >> block [
	^ methodGenerator 	block
]

{ #category : 'code generation' }
ASLLVMBlockOutput >> codeGeneratorClass [

	^ ASCodeGeneratorBlock
]

{ #category : 'compiling' }
ASLLVMBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : 'accessing' }
ASLLVMBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]

{ #category : 'accessing' }
ASLLVMBlockOutput >> prefix [

	^ 'B'
]

{ #category : 'visiting' }
ASLLVMBlockOutput >> visitReturnSelf [
	self nextPutAll: 'RETURNSELF';endStatement 
]
