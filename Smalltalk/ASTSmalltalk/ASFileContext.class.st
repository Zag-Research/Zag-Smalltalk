"
I am a writer of files.

I provide a context for CodeGenerators. I maintain a collection of them . The code generators will tell me about any symbols that need to be added.

"
Class {
	#name : 'ASFileContext',
	#superclass : 'Object',
	#instVars : [
		'components',
		'pendingComponents',
		'symbols',
		'objects'
	],
	#category : 'ASTSmalltalk-Contexts',
	#package : 'ASTSmalltalk',
	#tag : 'Contexts'
}

{ #category : 'components' }
ASFileContext >> addComponent: aGenerator [

	pendingComponents add: aGenerator.
	components add: aGenerator
]

{ #category : 'as yet unclassified' }
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

{ #category : 'as yet unclassified' }
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
