"
I am the superclass of all the operations that referencce the top elements of the stack
"
Class {
	#name : 'ASCStackReferencingOperations',
	#superclass : 'ASCOperations',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCStackReferencingOperations >> consumes: aStackElement [

	^ aStackElement location consumer: self
]

{ #category : 'data flow instructions' }
ASCStackReferencingOperations >> markReferences [

	^ stack markReferenced
]

{ #category : 'output' }
ASCStackReferencingOperations >> offsetOfSelf [

	^ (stack at: #self) offset - stack offset
]

{ #category : 'accessing' }
ASCStackReferencingOperations >> stack [
	^ stack
]

{ #category : 'as yet unclassified' }
ASCStackReferencingOperations >> theSelf [

	^ stack at: #self
]
