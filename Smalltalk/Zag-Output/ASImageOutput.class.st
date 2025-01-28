"
I output to an image
"
Class {
	#name : 'ASImageOutput',
	#superclass : 'ASOutput',
	#instVars : [
		'pendingComponents',
		'components',
		'objects'
	],
	#category : 'Zag-Output-Image-Output',
	#package : 'Zag-Output',
	#tag : 'Image-Output'
}

{ #category : 'adding' }
ASImageOutput >> addComponent: aGenerator [

	pendingComponents add: aGenerator.
	components add: aGenerator
]

{ #category : 'API' }
ASImageOutput >> codeGenerator [

	^ ASCodeGenerator new
]

{ #category : 'register' }
ASImageOutput >> for: anASNode generatorIs: anASGenerator [

	objects at: anASNode put: anASGenerator
]

{ #category : 'API' }
ASImageOutput >> generateCode: anASNode for: aClass [

	codeGenerator := self codeGenerator
		                 targetClass: aClass;
		                 generateFrom: anASNode.
	codeGenerator inline: true.
	codeGenerator finalAnalysis.
	self outputToStream
]

{ #category : 'initialization' }
ASImageOutput >> initialize [

	super initialize.
	pendingComponents := OrderedCollection new.
	components := IdentitySet new.
	objects := IdentityDictionary new.
	self context: self
]

{ #category : 'API' }
ASImageOutput >> literal: anObject [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASImageOutput >> name [

	^ componentName ifNil: [ componentName := ourObject selector ]
]

{ #category : 'components' }
ASImageOutput >> processComponents [

	| temp |
	[ pendingComponents isEmpty ] whileFalse: [
		temp := pendingComponents.
		pendingComponents := OrderedCollection new.
		temp do: [ :each | each generateCode ] ]
]

{ #category : 'visiting' }
ASImageOutput >> visitBranch: aBasicBlock [

	self
		branch;
		reference: aBasicBlock label
]

{ #category : 'API' }
ASImageOutput >> visitPushLiteral: aLiteral [

	self
		pushLiteral;
		literal: aLiteral
]

{ #category : 'API' }
ASImageOutput >> visitReturnTop: anObject [

	self returnTop
]
