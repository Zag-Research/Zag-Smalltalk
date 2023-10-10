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
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASZigCPSBlockOutput >> block [
	^ methodGenerator 	block
]

{ #category : #compiling }
ASZigCPSBlockOutput >> methodGenerator [

	^ methodGenerator
]

{ #category : #accessing }
ASZigCPSBlockOutput >> methodGenerator: anASZigCPSOutput block: anInteger [ 
	methodGenerator := anASZigCPSOutput. block := anInteger 

]

{ #category : #accessing }
ASZigCPSBlockOutput >> prefix [

	^ 'B'
]
