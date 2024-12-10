"
I go through all the methods in a class, looking for primitives calling methods
"
Class {
	#name : 'ASFindPrimitives',
	#superclass : 'Object',
	#instVars : [
		'class',
		'primitives'
	],
	#category : 'Zag-Core-Tests-Explore',
	#package : 'Zag-Core-Tests',
	#tag : 'Explore'
}

{ #category : 'instance creation' }
ASFindPrimitives >> on: aClass [

	class := aClass
]

{ #category : 'collecting' }
ASFindPrimitives >> primitives [

	| dict |
	primitives ifNotNil: [ ^ primitives ].
	dict := Dictionary new.
	class methodDict valuesDo: [ :method | 
		dict at: method primitive put: method ].
	dict removeKey: 0.
	^ primitives := dict associations asSortedCollection
]

{ #category : 'printing' }
ASFindPrimitives >> printEmbedded: s [

	self primitives do: [ :assoc | 
		| selector |
		selector := assoc value selector.
		s
			nextPutAll: '    pub fn @"';
			nextPutAll: selector;
			nextPutAll:
				'"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp['.
		selector numArgs printOn: s.
		s nextPutAll: '] = inlines.p'.
		assoc key printOn: s.
		s nextPutAll: '('.
		selector numArgs to: 0 by: -1 do: [ :n | 
			s nextPutAll: 'sp['.
			n printOn: s.
			s nextPutAll: ']'.
			n > 0 ifTrue: [ s nextPut: $, ] ].
		s
			nextPutAll:
				') catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"';
			nextPutAll: selector;
			nextPutAll: '" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + '.
		selector numArgs printOn: s.
		s nextPutAll: ', process, context, selector });
    }
' ]
]

{ #category : 'printing' }
ASFindPrimitives >> printInlines: s [

	self primitives do: [ :assoc | 
		| str |
		s nextPutAll: '    pub inline fn p'.
		assoc key printOn: s.
		s nextPutAll: '(self: Object'.
		str := ''.
		1 to: assoc value selector numArgs do: [ :n | 
			| id |
			id := 'other'.
			n > 1 ifTrue: [ id := id , n printString ].
			str := str , ',' , id.
			s
				nextPutAll: ', ';
				nextPutAll: id;
				nextPutAll: ': Object' ].
		s
			nextPutAll: ') !Object { // ';
			nextPutAll: assoc value selector;
			nextPutAll: '
        _ = .{self';
			nextPutAll: str;
			nextPutAll: '};
        return error.primitiveError;
    }
' ]
]

{ #category : 'printing' }
ASFindPrimitives >> printOn: s [

	s nextPutAll: 'const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub fn init() void {}

pub const inlines = struct {
'.
	self printInlines: s.
	s nextPutAll: '};
const fallback = execute.fallback;
pub const embedded = struct {
'.
	self printEmbedded: s.
	s nextPutAll: '};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
'.
	self printPrimitives: s.
	s nextPutAll: '};
'
]

{ #category : 'printing' }
ASFindPrimitives >> printPrimitives: s [

	self primitives do: [ :assoc | 
		s nextPutAll: '    pub fn p'.
		assoc key printOn: s.
		s
			nextPutAll:
				'(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // ';
			nextPutAll: assoc value selector;
			nextPutAll: '
        _ = .{ pc, sp, process, context, selector };
        @panic("unimplemented");
    }
' ]
]

{ #category : 'printing' }
ASFindPrimitives >> writeToTmpFile [

	(File named: '/tmp/' , class name , '.zig') writeStreamDo: [ :stream | 
		self printOn: (ZnNewLineWriterStream on: stream) ]
]
