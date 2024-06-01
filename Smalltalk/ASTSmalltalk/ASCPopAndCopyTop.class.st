Class {
	#name : 'ASCPopAndCopyTop',
	#superclass : 'ASCOperations',
	#instVars : [
		'objectsToPop'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCPopAndCopyTop >> with: anInteger [ 

	objectsToPop := anInteger
]
