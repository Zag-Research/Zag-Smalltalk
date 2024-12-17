"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturn',
	#superclass : 'ASCStackReferencingOperations',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'instance creation' }
ASCReturn >> with: ignored [

]
