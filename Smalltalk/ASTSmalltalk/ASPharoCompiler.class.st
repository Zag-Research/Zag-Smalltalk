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
ASPharoCompiler >> writeInitSmalltalk: aStream [ 
	aStream << 'var sym: Sym = undefined;'. aStream cr.
	aStream << 'fn initSmalltalk() void {'. aStream cr.
	aStream tab. aStream << 'primitives.init();'. aStream cr.
	aStream tab. aStream << 'sym = Sym.init();'. aStream cr.
	"write all the classes"
	"write the methods"
	methods do: [ :aMethod | aMethod writeMethod: aStream. ].
	
	aStream << '}'. aStream cr.
]

{ #category : #compiling }
ASPharoCompiler >> writeZig: aStream [ 
	self writeZigConsts: aStream.
	self writeZigSys: aStream.
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
ASPharoCompiler >> writeZigSys: aStream [ 
	aStream << 'const Sym = struct {' . aStream cr.
	selectors do: [ :selector | aStream tab. aStream << selector  asZigString << ': Object,'. aStream cr ].
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
