"
I know how to lookup class and global variables.
"
Class {
	#name : 'ASLookup',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASLookup >> lookupName: anObject block: blockOther [

	^ self error: 'Cannot find ', anObject
]
