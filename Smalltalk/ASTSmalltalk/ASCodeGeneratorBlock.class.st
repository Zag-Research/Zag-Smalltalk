"
I am a code generator for blocks... very minor but important differences from methods
"
Class {
	#name : #ASCodeGeneratorBlock,
	#superclass : #ASCodeGenerator,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #API }
ASCodeGeneratorBlock >> returnNonLocal [
	"need to return to the calling context's return"
	code returnNonLocal
]

{ #category : #API }
ASCodeGeneratorBlock >> returnSelf [
self error: 'need to push self'.
	^ code returnTopNonlocal
]

{ #category : #accessing }
ASCodeGeneratorBlock >> selfName [
	^ nil
]
