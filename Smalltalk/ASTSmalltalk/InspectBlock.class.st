"
I display information about a block closure in an inspect window. I show the operations in the block operations, the name, arguments and locals of a block.
"
Class {
	#name : 'InspectBlock',
	#superclass : 'Object',
	#instVars : [
		'ascBlock'
	],
	#category : 'ASTSmalltalk-Inspect',
	#package : 'ASTSmalltalk',
	#tag : 'Inspect'
}

{ #category : 'instance creation' }
InspectBlock class >> from: anASCBlock [
	^ self new from: anASCBlock 
]

{ #category : 'instance creation' }
InspectBlock >> children [ 
	^ ascBlock block body
]

{ #category : 'instance creation' }
InspectBlock >> from: anASCBlock [
	ascBlock := anASCBlock 
]

{ #category : 'instance creation' }
InspectBlock >> inspectZagASTOn: aStream [

	ascBlock inspectZagASTOn: aStream.
	"Daniel output the args and locals here"
]
