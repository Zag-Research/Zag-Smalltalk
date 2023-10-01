"
I am a writer of .zig files.

I provide a context for CodeGenerators. I maintain a collection of them as well as a symbol table. The code generators will tell me about any symbols that need to be added.

When asked to output, I output a header of all the external zag references, then I ask each of the generators for their definition and output that. Then I output the symbol table, and an initSmalltalk method. In there I initialize primitives and the symbol table, then I ask each of the generators for their initialization code. Finally I output a main function.
"
Class {
	#name : #ASZigFileContext,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'classNumber',
		'components',
		'stream',
		'labelNumber',
		'objects'
	],
	#classVars : [
		'Predefined'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #adding }
ASZigFileContext >> add: aGenerator [ 
	components add: aGenerator
]

{ #category : #testing }
ASZigFileContext >> addSymbol: aSymbol [

	symbols add: aSymbol
]

{ #category : #initialization }
ASZigFileContext >> initialize [

	super initialize.
	symbols := ASZigSymbols new.
	classNumber := 0.
	components := OrderedCollection new.
	labelNumber := 0.
	objects := Dictionary new
]

{ #category : #accessing }
ASZigFileContext >> lf [
	^ stream lf
]

{ #category : #accessing }
ASZigFileContext >> lookupObject: anObject [
	^ objects at: anObject ifAbsent: [ ^ nil ]
]

{ #category : #accessing }
ASZigFileContext >> nextPutAll: aString [ 
	^ stream nextPutAll: aString
]

{ #category : #wriiting }
ASZigFileContext >> outputTo: aWriteStream [

	aWriteStream truncate.
	stream := aWriteStream.
	self writeHeader.
	symbols writeDefinitionOn: stream.
	components do: [ :each | each writeDefinition ].
	self writeInitSmalltalkPrefix.
	symbols writeInitializationOn: stream.
	components do: [ :each | each writeInitialization ].
	self writeInitSmalltalkSuffix.
	self writeMainSuffix.
	stream flush.
	stream := nil
]

{ #category : #testing }
ASZigFileContext >> predefinedSymbol: aSymbol [

	^ (symbols predefinedSymbol: aSymbol)
]

{ #category : #testing }
ASZigFileContext >> printable: aSymbol [

	^ symbols printable: aSymbol
]

{ #category : #wriiting }
ASZigFileContext >> writeHeader [

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

{ #category : #wriiting }
ASZigFileContext >> writeInitSmalltalkPrefix [
	stream nextPutAll: 'fn initSmalltalk() void {
    primitives.init();
'
]

{ #category : #wriiting }
ASZigFileContext >> writeInitSmalltalkSuffix [

	stream nextPutAll: '}
pub fn main() !void {
    initSmalltalk();
'
]

{ #category : #wriiting }
ASZigFileContext >> writeMainSuffix [

	stream nextPutAll: '}
'
]

{ #category : #writing }
ASZigFileContext >> writeNewDeclaration [

	labelNumber := labelNumber + 1.
	^ self writeNewDeclaration: 'C' , labelNumber asString
]

{ #category : #writing }
ASZigFileContext >> writeNewDeclaration: label [

	stream
		nextPutAll: 'var ';
		nextPutAll: label;
		nextPutAll: ' = '.
	^ label
]
