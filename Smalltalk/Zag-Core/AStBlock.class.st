"
I represent a block closure object on the stack.

I can only represent myself, so even another block closure isn't compatible.
"
Class {
	#name : 'AStBlock',
	#superclass : 'AStackValue',
	#instVars : [
		'block',
		'compiledUnit'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStBlock class >> block: aBlock [
	
	^ self new block: aBlock.
]

{ #category : 'accessing' }
AStBlock >> ast [

	^ block
]

{ #category : 'accessing' }
AStBlock >> block: anASBlock [

	block := anASBlock
]

{ #category : 'compiling' }
AStBlock >> compileIn: aCodeGenerator [

	self halt.
	^ aCodeGenerator compileBlock: self
]

{ #category : 'accessing' }
AStBlock >> compiledUnit [
	^ compiledUnit
]

{ #category : 'accessing' }
AStBlock >> compiledUnit: aCompiledUnit [

	compiledUnit := aCompiledUnit
]

{ #category : 'as yet unclassified' }
AStBlock >> couldMergeIn: other [

	^ self == other
]

{ #category : 'API' }
AStBlock >> dataClass [

	^ ZagBlockClosure
]
