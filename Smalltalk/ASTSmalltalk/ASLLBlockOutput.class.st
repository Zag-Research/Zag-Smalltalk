"
I output BlockClosures
"
Class {
	#name : 'ASLLBlockOutput',
	#superclass : 'ASLLMethodOutput',
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
ASLLBlockOutput >> block [
	^ methodGenerator 	block
]

{ #category : 'code generation' }
ASLLBlockOutput >> codeGeneratorClass [

	^ ASCodeGeneratorBlock
]

{ #category : 'compiling' }
ASLLBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : 'accessing' }
ASLLBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]

{ #category : 'accessing' }
ASLLBlockOutput >> prefix [

	^ 'B'
]

{ #category : 'visiting' }
ASLLBlockOutput >> visitReturnSelf [
	self nextPutAll: 'RETURNSELF';endStatement 
]
