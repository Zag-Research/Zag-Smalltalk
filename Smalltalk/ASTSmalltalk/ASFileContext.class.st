"
I am a file context for code outputters.

I provide a context for CodeGenerators. I maintain a collection of them as well as a symbol table. The code generators will tell me about any symbols that need to be added.


"
Class {
	#name : 'ASFileContext',
	#superclass : 'Object',
	#instVars : [
		'components',
		'symbols',
		'pendingComponents',
		'objects'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'components' }
ASFileContext >> addComponent: aGenerator [

	pendingComponents add: aGenerator.
	components add: aGenerator
]

{ #category : 'testing' }
ASFileContext >> addSymbol: aSymbol [

	symbols add: aSymbol
]

{ #category : 'register' }
ASFileContext >> for: anASNode generatorIs: anASGenerator [ 
	objects at: anASNode put: anASGenerator 
]

{ #category : 'initialization' }
ASFileContext >> initialize [

	super initialize.
	components := OrderedCollection new.
	objects := Dictionary new.
	pendingComponents := OrderedCollection new
]

{ #category : 'accessing' }
ASFileContext >> lookupObject: anObject [

	^ objects at: anObject ifAbsent: [ ^ nil ]
]

{ #category : 'components' }
ASFileContext >> processComponents [

	| temp |
	[ pendingComponents isEmpty ] whileFalse: [
		temp := pendingComponents.
		pendingComponents := OrderedCollection new.
		temp do: [ :each | each generateCode ] ]
]
