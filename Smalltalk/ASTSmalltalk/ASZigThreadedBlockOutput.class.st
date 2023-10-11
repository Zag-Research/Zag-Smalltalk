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

{ #category : #accessing }
ASZigThreadedBlockOutput >> methodGenerator: anOutput block: anInteger [

	methodGenerator := anOutput.
	block := anInteger
]
