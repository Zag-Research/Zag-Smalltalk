"
I am a writer of .zig files.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : 'ASLLFileContext',
	#superclass : 'ASTextFileContext',
	#instVars : [
		'converter'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'initialization' }
ASLLFileContext >> initialize [

	super initialize.
	names := Dictionary new.
	converter := ASNanImage
]

{ #category : 'writing' }
ASLLFileContext >> writeHeaderOn: stream [

	stream nextPutAll:
		'@zag.process.Process = type { [909 x i64], ptr, i64, ptr, ptr, ptr, ptr, ptr, ptr, i64, [3637 x i64], [3637 x i64] }
@zag.context.Context = type { i64, ptr, @zag.execute.PC, ptr, ptr, i64, [0 x i64] }
@zag.execute.PC = type { ptr }
@zag.execute.Code = type { <{ ptr, [8 x i8] }> }
@zag.execute.Stack = type { i64, i64, i64, i64 }
@zag.execute.CompiledMethod = type { i64, i64, @zag.execute.MethodSignature, ptr, [0 x @zag.execute.Code] }
@zag.execute.MethodSignature = type { i32, i16, [2 x i8] }
'.
	#( nil false true ) do: [ :each |
		stream
			nextPut: $@;
			print: each;
			nextPutAll: ' = i64 0x';
			nextPutAll: ((converter convertToZag: each) printStringBase: 16);
			nextPutAll: '
' ]
]

{ #category : 'writing' }
ASLLFileContext >> writeInitSmalltalkPrefixOn: stream [

]

{ #category : 'writing' }
ASLLFileContext >> writeInitSmalltalkSuffixOn: stream [

]

{ #category : 'writing' }
ASLLFileContext >> writeMainPrefixOn: aStream [ 

]

{ #category : 'writing' }
ASLLFileContext >> writeNewDeclarationFor: aGenerator on: stream [

]
