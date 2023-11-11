"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : #ASCBlock,
	#superclass : #ASCOperations,
	#instVars : [
		'block',
		'manifest'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #asserting }
ASCBlock >> assertIsValue [

	
]

{ #category : #accessing }
ASCBlock >> block [
	^ block
]

{ #category : #accessing }
ASCBlock >> children [
	^ block body asOrderedCollection 
]

{ #category : #initialization }
ASCBlock >> initialize [ 
super initialize .
manifest := false
]

{ #category : #serializing }
ASCBlock >> makeManifest [ 	
		  manifest := true
]

{ #category : #accessing }
ASCBlock >> manifest [

^	manifest
]

{ #category : #accessing }
ASCBlock >> name [

	^ self
]

{ #category : #'code generation' }
ASCBlock >> outputTo: output [

manifest ifFalse: [ ^ output visitPushNil ].
	^ output visitPushBlock: self
]

{ #category : #'instance creation' }
ASCBlock >> with: anASBlock [

	block := anASBlock
]
