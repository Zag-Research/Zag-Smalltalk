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
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
'
]

{ #category : 'writing' }
ASZigFileContext >> writeInitSmalltalkPrefixOn: stream [

	stream 
	nextPutAll: 'var sym: Sym = undefined;';
	lf;
	nextPutAll: 'const e = primitives.embedded;';
	lf;
	nextPutAll: 'const p = primitives.primitives;';
	lf;
	nextPutAll: 'fn initSmalltalk() void {
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
	self halt.
	label := aGenerator name.
	stream
		nextPutAll: 'var ';
		nextPutAll: label zigName;
		nextPutAll: ' = '.
	^ label
]
