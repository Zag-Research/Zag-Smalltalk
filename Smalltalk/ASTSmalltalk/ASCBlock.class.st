"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : 'ASCBlock',
	#superclass : 'ASCOperations',
	#instVars : [
		'block',
		'manifest',
		'name'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'asserting' }
ASCBlock >> assertIsValue [

	
]

{ #category : 'accessing' }
ASCBlock >> block [
	^ block
]

{ #category : 'accessing' }
ASCBlock >> children [
	^ block body asOrderedCollection 
]

{ #category : 'initialization' }
ASCBlock >> initialize [ 
super initialize .
manifest := false
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
