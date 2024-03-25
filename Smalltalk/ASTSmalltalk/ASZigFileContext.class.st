"
I am a writer of .zig files.

I maintain a symbol table. The code generators will tell me about any symbols that need to be added.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : 'ASZigFileContext',
	#superclass : 'ASFileContext',
	#instVars : [
		'names'
	],
	#category : 'ASTSmalltalk-Contexts',
	#package : 'ASTSmalltalk',
	#tag : 'Contexts'
}

{ #category : 'testing' }
ASZigFileContext >> addSymbol: aSymbol [

	symbols add: aSymbol
]

{ #category : 'converting' }
ASZigFileContext >> asString [
^ String streamContents: [ :s | self outputTo: s ]
]

{ #category : 'initialization' }
ASZigFileContext >> initialize [

	super initialize.
	symbols := ASZigSymbols new.
	names := Dictionary new
]

{ #category : 'creation' }
ASZigFileContext >> name: anObject prefix: aString main: aString2 [

	^ self name: anObject prefix: aString main: aString2 seq: 0
]

{ #category : 'as yet unclassified' }
ASZigFileContext >> name: generator prefix: prefix main: main seq: seq [

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
ASZigFileContext >> outputTo: stream [

	self processComponents.
	self writeHeaderOn: stream.
	components do: [ :each | each writeDefinitionOn: stream ].
	symbols writeDefinitionOn: stream.
	self writeInitSmalltalkPrefixOn: stream.
	symbols writeInitializationOn: stream.
	components do: [ :each | each writeInitializationOn: stream ].
	self writeInitSmalltalkSuffixOn: stream.
	self writeMainPrefixOn: stream.
	self writeMainSuffixOn: stream.
	stream flush
]

{ #category : 'testing' }
ASZigFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : 'testing' }
ASZigFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]

{ #category : 'writing' }
ASZigFileContext >> writeHeaderOn: stream [

	stream nextPutAll: 'const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/zobject.zig").Object;
const ClassIndex = @import("zag/zobject.zig").ClassIndex;
const Nil = @import("zag/zobject.zig").Nil;
const True = @import("zag/zobject.zig").True;
const False = @import("zag/zobject.zig").False;
const indexSymbol = @import("zag/zobject.zig").indexSymbol;
const execute = @import("zag/execute.zig");
const tailCall = config.tailCall;
const Code = execute.Code;
const compileMethod = execute.compileMethod;
const compileObject = execute.compileObject;
const ContextPtr = execute.CodeContextPtr;
const TestExecution = execute.TestExecution;
const primitives = @import("zag/primitives.zig");
const Process = @import("zag/process.zig").Process;
const symbol = @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");

const empty = &[0]Object{};
'
]

{ #category : 'writing' }
ASZigFileContext >> writeInitSmalltalkPrefixOn: stream [

	stream nextPutAll: 'fn initSmalltalk() void {
    primitives.init();
'
]

{ #category : 'writing' }
ASZigFileContext >> writeInitSmalltalkSuffixOn: stream [

	stream nextPutAll: '}
'
]

{ #category : 'writing' }
ASZigFileContext >> writeMainPrefixOn: stream [

	stream nextPutAll: 'pub fn main() !void {
    initSmalltalk();
'
]

{ #category : 'writing' }
ASZigFileContext >> writeMainSuffixOn: stream [

	stream nextPutAll: '}
'
]

{ #category : 'writing' }
ASZigFileContext >> writeNewDeclarationFor: aGenerator on: stream [

	| label |
	label := aGenerator name.
	stream
		nextPutAll: 'var ';
		nextPutAll: label zigName;
		nextPutAll: ' = '.
	^ label
]
