"
I output BlockClosures
"
Class {
	#name : #ASZigThreadedBlockOutput,
	#superclass : #ASZigThreadedOutput,
	#instVars : [
		'methodGenerator',
		'block'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASZigThreadedBlockOutput >> codeGeneratorClass [

	^ ASCodeGeneratorBlock
]

{ #category : #compiling }
ASZigThreadedBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : #accessing }
ASZigThreadedBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]

{ #category : #visiting }
ASZigThreadedBlockOutput >> visitPushLocalOrBlock: anObject [ 
	^ self subclassResponsibility
]

{ #category : #visiting }
ASZigThreadedBlockOutput >> visitStoreLocal: anASLocal [ 
	^ self subclassResponsibility
]
