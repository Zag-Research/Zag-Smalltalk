"
I am an object initally allocate on a stack, but then possible moved to a heap.
"
Class {
	#name : 'ASIStackHeapObject',
	#superclass : 'Object',
	#instVars : [
		'stackOrArray',
		'offset',
		'size',
		'isOnStack'
	],
	#category : 'ASTSmalltalk-Tests-SemanticInterpreter',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'as yet unclassified' }
ASIStackHeapObject >> array: aCollection offset: anInteger size: aSize [

	stackOrArray := aCollection.
	offset := anInteger.
	size := aSize.
	isOnStack := true
]

{ #category : 'as yet unclassified' }
ASIStackHeapObject >> at: position [

	^ stackOrArray at: position + offset
]

{ #category : 'as yet unclassified' }
ASIStackHeapObject >> isOnStack [

	^ isOnStack
]

{ #category : 'as yet unclassified' }
ASIStackHeapObject >> toHeap [

	stackOrArray := (Array new: size)
		                replaceFrom: 1
		                to: size
		                with: stackOrArray
		                startingAt: offset.
	offset := 1.
	isOnStack := false
]
