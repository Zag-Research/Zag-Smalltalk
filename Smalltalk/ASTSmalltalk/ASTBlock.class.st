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
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
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

{ #category : 'as yet unclassified' }
AStBlock >> dataClass [

	^ BlockClosure
]
