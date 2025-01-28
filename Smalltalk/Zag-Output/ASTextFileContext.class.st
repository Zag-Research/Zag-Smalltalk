"
I am the general text file context.

I have subclasses that capture language-specific structure and syntax
"
Class {
	#name : 'ASTextFileContext',
	#superclass : 'ASFileContext',
	#instVars : [
		'names'
	],
	#category : 'Zag-Output-Text-Output',
	#package : 'Zag-Output',
	#tag : 'Text-Output'
}

{ #category : 'converting' }
ASTextFileContext >> asString [
^ String streamContents: [ :s | self outputTo: s ]
]

{ #category : 'initialization' }
ASTextFileContext >> initialize [

	super initialize.
	symbols := ASZagSymbols new.

]

{ #category : 'creation' }
ASTextFileContext >> name: anObject prefix: aPrefix main: aSymbol [
^ (self name: anObject prefix: aPrefix main: aSymbol seq: 0)
]

{ #category : 'creation' }
ASTextFileContext >> name: generator prefix: prefix main: main seq: seq [

	| string |
	string := prefix , (seq = 0
		           ifTrue: [
			           main isEmptyOrNil
				           ifTrue: [ '1' ]
				           ifFalse: [ '' ] ]
		           ifFalse: [ seq asString ]) , (main ifNil: ['']).
	names at: string ifPresent: [
		^ self
			  name: generator
			  prefix: prefix
			  main: main
			  seq: seq + 1 ].
	names at: string put: generator.
	^ string
]

{ #category : 'writing' }
ASTextFileContext >> outputTo: stream [

	self processComponents.
	self writeHeaderOn: stream.
	components do: [ :each | each writeDefinitionOn: stream ].
	symbols writeDefinitionOn: self.
	self writeInitSmalltalkPrefixOn: stream.
	symbols writeInitializationOn: self.
	components do: [ :each | each writeInitializationOn: stream ].
	self writeInitSmalltalkSuffixOn: stream.
	self writeMainPrefixOn: stream.
	self writeMainSuffixOn: stream.
	stream flush
]

{ #category : 'testing' }
ASTextFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : 'testing' }
ASTextFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]

{ #category : 'writing' }
ASTextFileContext >> writeHeaderOn: stream [

	^ self subclassResponsibility
]

{ #category : 'writing' }
ASTextFileContext >> writeInitSmalltalkPrefixOn: stream [

	^ self subclassResponsibility
]

{ #category : 'writing' }
ASTextFileContext >> writeInitSmalltalkSuffixOn: stream [

	^ self subclassResponsibility
]

{ #category : 'writing' }
ASTextFileContext >> writeMainPrefixOn: stream [

	^ self subclassResponsibility
]

{ #category : 'writing' }
ASTextFileContext >> writeMainSuffixOn: aStream [

]

{ #category : 'writing' }
ASTextFileContext >> writeNewDeclarationFor: aGenerator on: stream [ 
	^ self subclassResponsibility
]
