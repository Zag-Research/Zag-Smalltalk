"
I am a program counter that will simply end interpretation
"
Class {
	#name : 'ASINullProgramCounter',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'interpreting' }
ASINullProgramCounter >> doForInterpreter: anInterpreter [

	^ anInterpreter endInterpretation
]
