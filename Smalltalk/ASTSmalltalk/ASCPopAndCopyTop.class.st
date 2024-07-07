"
I drop n elements from below the top of stack
"
Class {
	#name : 'ASCPopAndCopyTop',
	#superclass : 'ASCStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'data flow instructions' }
ASCPopAndCopyTop >> markReferences [

	stack markReferenced
]

{ #category : 'stack' }
ASCPopAndCopyTop >> resultStack: anASStack [

	stack := anASStack.
	^ anASStack dropUnder: (stack offsetToSelf: 0) - 1
]

{ #category : 'instance creation' }
ASCPopAndCopyTop >> with: ignored [
]
