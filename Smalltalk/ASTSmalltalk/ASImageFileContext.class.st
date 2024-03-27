"
I am a writer of .zag files.

I maintain a symbol table. The code generators will tell me about any symbols that need to be added.

When asked to output, I output an image file.
"
Class {
	#name : #ASImageFileContext,
	#superclass : #ASFileContext,
	#category : 'ASTSmalltalk-Contexts'
}

{ #category : #testing }
ASImageFileContext >> addSymbol: aSymbol [

	symbols add: aSymbol
]

{ #category : #converting }
ASImageFileContext >> asString [
^ String streamContents: [ :s | self outputTo: s ]
]

{ #category : #initialization }
ASImageFileContext >> initialize [

	super initialize.
	symbols := ASZigSymbols new.

]

{ #category : #creation }
ASImageFileContext >> name: anObject prefix: aString main: aString2 [

	^ self name: anObject prefix: aString main: aString2 seq: 0
]

{ #category : #writing }
ASImageFileContext >> outputTo: stream [

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

{ #category : #testing }
ASImageFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : #testing }
ASImageFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]

{ #category : #writing }
ASImageFileContext >> writeHeaderOn: stream [

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

{ #category : #writing }
ASImageFileContext >> writeInitSmalltalkPrefixOn: stream [

	stream nextPutAll: 'fn initSmalltalk() void {
    primitives.init();
'
]

{ #category : #writing }
ASImageFileContext >> writeInitSmalltalkSuffixOn: stream [

	stream nextPutAll: '}
'
]

{ #category : #writing }
ASImageFileContext >> writeMainPrefixOn: stream [

	stream nextPutAll: 'pub fn main() !void {
    initSmalltalk();
'
]

{ #category : #writing }
ASImageFileContext >> writeMainSuffixOn: stream [

	stream nextPutAll: '}
'
]

{ #category : #writing }
ASImageFileContext >> writeNewDeclarationFor: aGenerator on: stream [

	| label |
	label := aGenerator name.
	stream
		nextPutAll: 'var ';
		nextPutAll: label zigName;
		nextPutAll: ' = '.
	^ label
]
