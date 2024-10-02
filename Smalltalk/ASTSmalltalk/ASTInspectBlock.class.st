"
I display information about a block closure in an inspect window. I show the operations in the block operations, the name, arguments and locals of a block.
"
Class {
	#name : 'ASTInspectBlock',
	#superclass : 'Object',
	#instVars : [
		'ascBlock'
	],
	#category : 'ASTSmalltalk-Inspect',
	#package : 'ASTSmalltalk',
	#tag : 'Inspect'
}

{ #category : 'instance creation' }
ASTInspectBlock class >> from: anASCBlock [
	^ self new from: anASCBlock 
]

{ #category : 'instance creation' }
ASTInspectBlock >> children [ 
	^ ascBlock scope
]

{ #category : 'instance creation' }
ASTInspectBlock >> from: anASCBlock [
	ascBlock := anASCBlock 
]

{ #category : 'printing' }
ASTInspectBlock >> inspectZagASTOn: aStream [

	ascBlock inspectZagASTOn: aStream.
	"Daniel output the args and locals here"
]
