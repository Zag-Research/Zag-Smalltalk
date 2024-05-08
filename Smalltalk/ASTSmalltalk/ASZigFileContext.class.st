"
I am a writer of .zig files.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : 'ASZigFileContext',
	#superclass : 'ASTextFileContext',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'initialization' }
ASZigFileContext >> initialize [

	super initialize.
	names := Dictionary new
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

{ #category : 'as yet unclassified' }
ASZigFileContext >> writeMainPrefixOn: stream [

	stream nextPutAll: 'pub fn main() !void {
    initSmalltalk();
'
]

{ #category : 'as yet unclassified' }
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
