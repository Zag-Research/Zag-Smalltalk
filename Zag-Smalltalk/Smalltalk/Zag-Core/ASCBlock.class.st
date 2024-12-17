"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : 'ASCBlock',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'block'
	],
	#classInstVars : [
		'index'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'asserting' }
ASCBlock >> assertIsValue [

	
]

{ #category : 'accessing' }
ASCBlock >> block [
	^ block
]

{ #category : 'accessing' }
ASCBlock >> inferType: anASCodeGenerator [

	^ BlockClosure new "anASCodeGenerator targetClass"
]

{ #category : 'printing' }
ASCBlock >> inspectZagASTOn: aStream [

	"super inspectZagASTOn: aStream."
	block inspectZagASTOn: aStream
	"aStream << block asBlock body printString"
]

{ #category : 'testing' }
ASCBlock >> isBlock [
	^ true
]

{ #category : 'testing' }
ASCBlock >> needsContext: aStackContext [

	^ true
]

{ #category : 'code generation' }
ASCBlock >> outputTo: output [
	"seems like we should determin if this is a nonLocal return block"

	^ output visitPushBlock: self
]

{ #category : 'stack' }
ASCBlock >> resultStack: anASStack basicBlock: aBasicBlock [
	"	block parent: aBasicBlock."

	stack := anASStack.
	^ self createsTemporary: anASStack data: block
]

{ #category : 'instance creation' }
ASCBlock >> with: anASBlock [

	block := anASBlock
]
