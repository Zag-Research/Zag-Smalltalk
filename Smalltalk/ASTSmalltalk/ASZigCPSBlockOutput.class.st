"
I output BlockClosures
"
Class {
	#name : #ASZigCPSBlockOutput,
	#superclass : #ASZigCPSOutput,
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : 'ASTSmalltalk-Output'
}

{ #category : #accessing }
ASZigCPSBlockOutput >> block [
	^ methodGenerator 	block
]

{ #category : #'code generation' }
ASZigCPSBlockOutput >> codeGeneratorClass [

	^ ASCodeGeneratorBlock
]

{ #category : #compiling }
ASZigCPSBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : #accessing }
ASZigCPSBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]

{ #category : #accessing }
ASZigCPSBlockOutput >> prefix [

	^ 'B'
]

{ #category : #visiting }
ASZigCPSBlockOutput >> visitReturnSelf [
	self nextPutAll: 'RETURNSELF';endStatement 
]
