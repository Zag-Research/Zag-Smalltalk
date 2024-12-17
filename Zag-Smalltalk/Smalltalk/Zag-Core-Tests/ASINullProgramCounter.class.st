"
I am a program counter that will simply end interpretation
"
Class {
	#name : 'ASINullProgramCounter',
	#superclass : 'Object',
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'interpreting' }
ASINullProgramCounter >> doForInterpreter: anInterpreter [

	^ anInterpreter endInterpretation
]
