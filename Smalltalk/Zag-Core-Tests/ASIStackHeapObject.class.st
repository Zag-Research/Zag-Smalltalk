"
I am an object initally allocate on a stack, but then possible moved to a heap.
"
Class {
	#name : 'ASIStackHeapObject',
	#superclass : 'ASIObject',
	#instVars : [
		'stackOrArray',
		'offset',
		'size',
		'isOnStack'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'initialization' }
ASIStackHeapObject >> array: aCollection offset: anInteger size: aSize [

	stackOrArray := aCollection.
	offset := anInteger.
	size := aSize.
	isOnStack := true
]

{ #category : 'accessing' }
ASIStackHeapObject >> at: position [

	^ stackOrArray at: position + offset
]

{ #category : 'accessing' }
ASIStackHeapObject >> at: position put: anObject [

	^ stackOrArray at: position + offset put: anObject
]

{ #category : 'initialization' }
ASIStackHeapObject >> initialize [

	super initialize.
	isOnStack := false
]

{ #category : 'testing' }
ASIStackHeapObject >> isOnStack [

	^ isOnStack
]

{ #category : 'accessing' }
ASIStackHeapObject >> offset [

	^ offset
]

{ #category : 'transforming' }
ASIStackHeapObject >> toHeap [

	stackOrArray := (Array new: size)
		                replaceFrom: 1
		                to: size
		                with: stackOrArray
		                startingAt: offset + 1.
	offset := 0.
	isOnStack := false
]
