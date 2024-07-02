"
I represent an embedded send replacement.
"
Class {
	#name : 'ASCEmbed',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'selector'
	],
	#classVars : [
		'EmbeddingReturnTypes'
	],
	#classInstVars : [
		'embeddings'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
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
		              (SmallInteger >> #<).
		              (SmallInteger >> #+) }.

]

{ #category : 'API' }
ASCEmbed class >> isEmbedded: aCompileMethod [
 	"better to just check if this method contains a primitive and not in the list"
	^ embeddings hasLiteral: aCompileMethod 
]

{ #category : 'printing' }
ASCEmbed >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector.
]

{ #category : 'data flow instructions' }
ASCEmbed >> markReferences [

	stack markReferenced: selector numArgs
]

{ #category : 'instance creation' }
ASCEmbed >> next [
	^nil
]

{ #category : 'code generation' }
ASCEmbed >> outputTo: anOutput [

	^ anOutput visitEmbed: selector
]

{ #category : 'stack' }
ASCEmbed >> resultStack: anASStack [

	stack := anASStack.
	^ (anASStack drop: selector numArgs + 1) push:
		  (EmbeddingReturnTypes
			   at: selector
			   ifAbsent: [ ASCUnknownValue new ])
]

{ #category : 'instance creation' }
ASCEmbed >> with: aSymbol [

	selector := aSymbol
]
