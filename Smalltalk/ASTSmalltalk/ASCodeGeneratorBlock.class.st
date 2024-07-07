"
I am a code generator for blocks... very minor but important differences from methods
"
Class {
	#name : 'ASCodeGeneratorBlock',
	#superclass : 'ASCodeGenerator',
	#instVars : [
		'parent',
		'source'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCodeGeneratorBlock >> children [ 
	^ code children 
]

{ #category : 'API' }
ASCodeGeneratorBlock >> find: aSymbol [

	| indirect |

	indirect := ASCIndirectVariable new.
	indirect name: aSymbol.
	^ indirect variable: (parentCodeGenerator find: aSymbol)
]

{ #category : 'accessing' }
ASCodeGeneratorBlock >> inferType: anExpression [
self halt.
	^ parentCodeGenerator inferType: anExpression
]

{ #category : 'initialization' }
ASCodeGeneratorBlock >> initCode [

	code := ASCBlockClosureBlock new
]

{ #category : 'initialization' }
ASCodeGeneratorBlock >> initialize [ 
	super initialize.
"	code makeBlockClosure"
]

{ #category : 'printing' }
ASCodeGeneratorBlock >> inspectZagASTOn: stream [
	source inspectZagASTOn: stream
]

{ #category : 'as yet unclassified' }
ASCodeGeneratorBlock >> parent: anASCodeGenerator source: aBlockClosureValue ast: anASBlock [

	self flag: 'todo'.
	parent := anASCodeGenerator.
	source := aBlockClosureValue.
	self generateFrom: anASBlock
]

{ #category : 'API' }
ASCodeGeneratorBlock >> returnNonLocal [
	"need to return to the calling context's return"
	code returnNonLocal
]

{ #category : 'API' }
ASCodeGeneratorBlock >> returnSelf [
self error: 'need to push self'.
	^ code returnTopNonlocal
]

{ #category : 'accessing' }
ASCodeGeneratorBlock >> selfName [
	^ nil
]
