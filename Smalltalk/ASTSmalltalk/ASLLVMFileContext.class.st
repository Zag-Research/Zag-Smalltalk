"
I am a writer of .zig files.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : 'ASLLVMFileContext',
	#superclass : 'ASTextFileContext',
	#instVars : [
		'names'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'initialization' }
ASLLVMFileContext >> initialize [

	super initialize.
	names := Dictionary new
]

{ #category : 'creation' }
ASLLVMFileContext >> name: anObject prefix: aString main: aString2 [

	^ self name: anObject prefix: aString main: aString2 seq: 0
]

{ #category : 'creation' }
ASLLVMFileContext >> name: generator prefix: prefix main: main seq: seq [

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
ASLLVMFileContext >> outputTo: stream [

	self processComponents.
	self writeHeaderOn: stream.
	components do: [ :each | each writeDefinitionOn: stream ].
	symbols writeDefinitionOn: stream.
	self writeInitSmalltalkPrefixOn: stream.
	symbols writeInitializationOn: stream.
	components do: [ :each | each writeInitializationOn: stream ].
	self writeInitSmalltalkSuffixOn: stream.
	stream flush
]

{ #category : 'testing' }
ASLLVMFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : 'testing' }
ASLLVMFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]

{ #category : 'writing' }
ASLLVMFileContext >> writeHeaderOn: stream [

	stream nextPutAll: ''
]

{ #category : 'writing' }
ASLLVMFileContext >> writeInitSmalltalkPrefixOn: stream [

	stream nextPutAll: 'fn initSmalltalk() void {
    primitives.init();
'
]

{ #category : 'writing' }
ASLLVMFileContext >> writeInitSmalltalkSuffixOn: stream [

	stream nextPutAll: '}
'
]

{ #category : 'writing' }
ASLLVMFileContext >> writeNewDeclarationFor: aGenerator on: stream [

	| label |
	label := aGenerator name.
	stream
		nextPutAll: 'var ';
		nextPutAll: label zigName;
		nextPutAll: ' = '.
	^ label
]
