"
I will take a class in the current image and compile it to methods of ASTs
"
Class {
	#name : #ASPharoCompiler,
	#superclass : #ASCompiler,
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #compiling }
ASPharoCompiler >> compileMethod: aCompiledMethod [ 
	^ (ASPharoVisitor compiler: self) visit: aCompiledMethod ast
]

{ #category : #compiling }
ASPharoCompiler >> initializeSyms [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"
	syms := Dictionary new.
	syms at: 'yourself' put: 'Sym.yourself'.
   syms at: 'doesNotUnderstand:' put: 'Sym.@"doesNotUnderstand:"'.
   syms at: '=' put: 'Sym.@"="'.
   syms at: '+' put: 'Sym.@"+"'.
   syms at: '-' put: 'Sym.@"-"'.
   syms at: '*' put: 'Sym.@"*"'.
   syms at: 'size' put: 'Sym.size'.
   syms at: 'at:' put: 'Sym.@"at:"'.
   syms at: 'at:put:' put: 'Sym.@"at:put:"'.
   syms at: '~=' put: 'Sym.@"~="'.
   syms at: '==' put: 'Sym.@"=="'.
   syms at: '~~' put: 'Sym.@"~~"'.
   syms at: 'value' put: 'Sym.value'.
   syms at: 'value:' put: 'Sym.@"value:"'.
   syms at: 'value:value:' put: 'Sym.@"value:value:"'.
   syms at: 'negated' put: 'Sym.negated'.
   syms at: 'new' put: 'Sym.new'.
   syms at: 'new:' put: 'Sym.@"new:"'.
   syms at: 'cull:' put: 'Sym.@"cull:"'.
   syms at: 'value:value:value:' put: 'Sym.@"value:value:value:"'.
   syms at: 'value:value:value:value:' put: 'Sym.@"value:value:value:value:"'.
   syms at: 'valueWithArguments:' put: 'Sym.@"valueWithArguments:"'.
   syms at: 'cull:cull:' put: 'Sym.@"cull:cull:"'.
   syms at: 'cull:cull:cull:' put: 'Sym.@"cull:cull:cull:"'.
   syms at: 'cull:cull:cull:cull:' put: 'Sym.@"cull:cull:cull:cull:"'.
   syms at: 'self' put: 'Sym.self'.
   syms at: 'name' put: 'Sym.name'.
   syms at: '<' put: 'Sym.@"<"'.
   syms at: '<=' put: 'Sym.@"<="'.
   syms at: '>=' put: 'Sym.@">="'.
   syms at: '>' put: 'Sym.@">"'.
   syms at: 'class' put: 'Sym.class'.
   syms at: 'Class' put: 'Sym.Class'.
   syms at: 'Behavior' put: 'Sym.Behavior'.
   syms at: 'ClassDescription' put: 'Sym.ClassDescription'.
   syms at: 'Metaclass' put: 'Sym.Metaclass'.
   syms at: 'SmallInteger' put: 'Sym.SmallInteger'.
   syms at: 'noFallback' put: 'Sym.noFallback'.
   syms at: 'ifTrue:' put: 'Sym.@"ifTrue:"'.
   syms at: 'ifTrue:ifFalse' put: 'Sym.@"ifTrue:ifFalse"'.
   syms at: 'ifFalse:' put: 'Sym.@"ifFalse:"'.
   syms at: 'ifFalse:ifTrue:' put: 'Sym.@"ifFalse:ifTrue:"'.
   syms at: 'ifNil:' put: 'Sym.@"ifNil:"'.
   syms at: 'ifNil:ifNotNil' put: 'Sym.@"ifNil:ifNotNil"'.
   syms at: 'ifNotNil:' put: 'Sym.@"ifNotNil:"'.
   syms at: 'ifNotNil:ifNil:' put: 'Sym.@"ifNotNil:ifNil:"'.
   syms at: 'Object' put: 'Sym.Object'
]

{ #category : #compiling }
ASPharoCompiler >> writeInitSmalltalk: aStream [

	aStream << 'var sym: Sym = undefined;'.
	aStream cr.
	"write the methods"
	methods do: [ :aMethod | aMethod writeMethod: aStream with: syms ].
	aStream cr.
	aStream << 'fn initSmalltalk() void {'.
	aStream cr.
	aStream tab.
	aStream << 'primitives.init();'.
	aStream cr.
	aStream tab.
	aStream << 'sym = Sym.init();'.
	aStream cr.
	"write all the classes"
	methods do: [ :aMethod | aMethod setLiterals: aStream ].

	aStream << '}'.
	aStream cr
]

{ #category : #compiling }
ASPharoCompiler >> writeZig: aStream [ 
	self initializeSyms.
	self writeZigConsts: aStream.
	self writeZigSyms: aStream.
	self writeInitSmalltalk: aStream
]

{ #category : #compiling }
ASPharoCompiler >> writeZigConsts: aStream [ 
	aStream << 'const std = @import("std");'. aStream cr.
   aStream << 'const debug = std.debug;'. aStream cr.
   aStream << 'const math = std.math;' . aStream cr.
   aStream << 'const stdout = std.io.getStdOut().writer();' . aStream cr.
   aStream << 'const Object = @import("zag/zobject.zig").Object;' . aStream cr.
   aStream << 'const Nil = @import("zag/zobject.zig").Nil;' . aStream cr.
   aStream << 'const indexSymbol = $@import($"zag/zobject.zig$").indexSymbol;' . aStream cr.
   aStream << 'const execute = $@import($"zag/execute.zig$");' . aStream cr.
   aStream << 'const tailCall = execute.tailCall;' . aStream cr.
   aStream << 'const Code = execute.Code;' . aStream cr.
   aStream << 'const compileMethod = execute.compileMethod;' . aStream cr.
   aStream << 'const ContextPtr = execute.CodeContextPtr;' . aStream cr.
   aStream << 'const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;' . aStream cr.
   aStream << 'const TestExecution = execute.TestExecution;' . aStream cr.
   aStream << 'const primitives = @import("zag/primitives.zig");' . aStream cr.
   aStream << 'const Process = @import("zag/process.zig").Process;' . aStream cr.
   aStream << 'const symbol =  @import("zag/symbol.zig");' . aStream cr.
   aStream << 'const heap =  @import("zag/heap.zig");' . aStream cr.
   aStream cr
]

{ #category : #compiling }
ASPharoCompiler >> writeZigSyms: aStream [
	aStream << 'const Sym = struct {' . aStream cr.
	selectors do: [ :selector | aStream tab. aStream << selector asZigString << ': Object,'. aStream cr ].
	aStream tab. aStream << 'const ss = heap.compileStrings(.{'. aStream cr.
	selectors do: [ :aSelector | aStream tab. aStream tab. aStream << '"' << aSelector << '",'. aStream cr ].
	aStream tab. aStream << '});'. aStream cr.
	aStream tab. aStream << 'usingnamespace symbol.symbols;'. aStream cr.
	aStream tab. aStream << 'fn init() Sym {'. aStream cr.
	aStream tab. aStream tab. aStream << 'fn init() Sym {'. aStream cr.
	selectors withIndexDo: [ :aSelector :index | aStream tab. aStream tab. aStream tab. aStream << '.' << aSelector asZigString << ' = symbol.intern(ss['  << (index-1) asString << '].asObject()),'. aStream cr ].
	aStream tab. aStream tab. aStream << '}'. aStream cr.
	aStream tab. aStream << '};'. aStream cr.
	aStream << '};' . aStream cr.
   aStream cr
]
