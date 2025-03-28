"
I am the output for Blocks for LLVM
"
Class {
	#name : 'ASLLBlockOutput',
	#superclass : 'ASLLMethodOutput',
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
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
