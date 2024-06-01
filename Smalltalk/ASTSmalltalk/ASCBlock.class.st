"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : 'ASCBlock',
	#superclass : 'ASCOperations',
	#instVars : [
		'block',
		'manifest',
		'name',
		'label'
	],
	#classInstVars : [
		'index'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'label' }
ASCBlock class >> labelIndex [

	index ifNil: [ index := 0 ].
	index := index + 1.
	^ index
]

{ #category : 'label' }
ASCBlock class >> resetLabelIndex [

 index := 0
]

{ #category : 'asserting' }
ASCBlock >> assertIsValue [

	
]

{ #category : 'accessing' }
ASCBlock >> block [
	^ block
]

{ #category : 'accessing' }
ASCBlock >> inferType: anASCodeGenerator [

	^ ASTBlockClosure "anASCodeGenerator targetClass"
]

{ #category : 'initialization' }
ASCBlock >> initialize [
	super initialize.
	manifest := false
]

{ #category : 'instance creation' }
ASCBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: label.
	aStream << $)
]

{ #category : 'accessing' }
ASCBlock >> label [
	label ifNil: [ label := self class labelIndex asString, 'mref' ].
	^ label
	
]

{ #category : 'serializing' }
ASCBlock >> makeManifest [
	"a block becomes manifest for one of two reasons, 
		1. if the block contains a pops or stores into a variable
		2. if the block contains a send"
	manifest := true
]

{ #category : 'accessing' }
ASCBlock >> manifest [

^	manifest
]

{ #category : 'accessing' }
ASCBlock >> name [

	^ name
]

{ #category : 'accessing' }
ASCBlock >> name: aSymbol [
	name := aSymbol
]

{ #category : 'code generation' }
ASCBlock >> outputTo: output [
"seems like we should determin if this is a nonLocal return block"

manifest ifFalse: [ ^ output visitPushNil ].
	^ output visitPushBlock: self
]

{ #category : 'instance creation' }
ASCBlock >> with: anASBlock [

	block := anASBlock
]
