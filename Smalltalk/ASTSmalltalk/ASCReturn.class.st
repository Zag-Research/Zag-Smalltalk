"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturn',
	#superclass : 'ASCStackReferencingOperations',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCReturn >> with: ignored [

]
