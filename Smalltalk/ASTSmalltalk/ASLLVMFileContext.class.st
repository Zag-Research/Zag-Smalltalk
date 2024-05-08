"
I am a writer of .zig files.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : 'ASLLVMFileContext',
	#superclass : 'ASTextFileContext',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'initialization' }
ASLLVMFileContext >> initialize [

	super initialize.
	names := Dictionary new
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
