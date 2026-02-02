"
I represent an embedded send replacement.
"
Class {
	#name : 'ASCEmbed',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'selector',
		'primitive'
	],
	#classVars : [
		'EmbeddingReturnTypes'
	],
	#classInstVars : [
		'embeddings'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'class initialization' }
ASCEmbed class >> initialize [

	EmbeddingReturnTypes := Dictionary new.
	EmbeddingReturnTypes at: #- put: SmallInteger.
	EmbeddingReturnTypes at: #'<=' put: Boolean.
	EmbeddingReturnTypes at: #< put: Boolean.
	EmbeddingReturnTypes at: #+ put: SmallInteger.

	embeddings := {
		              (Integer >> #-).
		              (Integer >> #'<=').
		              (SmallInteger >> #'<=').
		              (SmallInteger >> #<).
		              (SmallInteger >> #+).
		              (SmallInteger >> #-) }
]

{ #category : 'API' }
ASCEmbed class >> isEmbedded: aCompileMethod [
 	"better to just check if this method contains a primitive and not in the list"
	^ embeddings hasLiteral: aCompileMethod 
]

{ #category : 'instance creation' }
ASCEmbed >> fromOperation: anASCSend [

	stack := anASCSend stack
]

{ #category : 'printing' }
ASCEmbed >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector.
]

{ #category : 'as yet unclassified' }
ASCEmbed >> interpretFor: anASemanticInterpreter [ 
		
	| numArgs args target |

	"consume the num args off the stack"
	numArgs := selector numArgs.
	args := OrderedCollection new.
	[ numArgs > 0 ] whileTrue: [ args add: anASemanticInterpreter pop. numArgs := numArgs -1].
	target := anASemanticInterpreter pop.
	
	"perform the primitive
	put the result of primitive on stack"
	anASemanticInterpreter push: (target perform: selector  withArguments: args )
]

{ #category : 'instance creation' }
ASCEmbed >> nextBasicBlocks [
	^nil
]

{ #category : 'code generation' }
ASCEmbed >> outputTo: anOutput [

	^ anOutput visitEmbed: selector
]

{ #category : 'writing' }
ASCEmbed >> primitive: anInteger [

	primitive := anInteger
]

{ #category : 'stack' }
ASCEmbed >> resultStack: anASStack [

	self halt
]

{ #category : 'accessing' }
ASCEmbed >> selector: aString [ 
	selector := aString
]

{ #category : 'instance creation' }
ASCEmbed >> with: aSymbol [

	selector := aSymbol
]
