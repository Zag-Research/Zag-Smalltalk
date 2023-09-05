Class {
	#name : #ASZigFileGenerator,
	#superclass : #ASZigGenerator,
	#instVars : [
		'stream',
		'instructions',
		'symbols'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #accessing }
ASZigFileGenerator class >> stream: aStream [
 ^ self new stream: aStream
]

{ #category : #accessing }
ASZigFileGenerator >> comment: aComment [

	instructions add: '// ' , aComment
]

{ #category : #accessing }
ASZigFileGenerator >> emitClosure: stackIndex modifier: size [
stream tab.
		stream << '&e.closureData, '
		<< stackIndex asString << ' + ('
		<< size asString
		<< ' << 8), // local:3 size:1 (offset 1 is l1)'.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitFooter: methods [

	stream << 'fn initSmalltalk() void {'.
	stream cr.
	stream tab.
	stream << 'const empty = &[0]Object{};'.
	stream cr.
	stream tab.
	stream << 'dispatch.init();'.
	stream cr.
	stream tab.
	stream << 'primitives.init();'.
	stream cr.
	stream tab.
	stream << 'sym = Sym.init();'.
	stream cr.
	"write all the classes"
	methods do: [ :aMethod | aMethod writeLiterals: stream with: symbols ].
	methods do: [ :aMethod | aMethod writeDispatch: stream with: symbols ].

	stream << '}'.
	stream cr.
	stream << 'const i = @import("zag/primitives.zig").inlines;'.
	stream cr.
	stream << 'const e = @import("zag/primitives.zig").embedded;'.
	stream cr.
	stream << 'const p = @import("zag/primitives.zig").primitives;'.

	stream close
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitHeader: selectors [
	self emitZigConstants.
	symbols := self initializeZigSymbols.
	self emitZigSymbols: symbols selectors: selectors
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitInstruction: instruction [
stream tab.
		stream << instruction.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitMethod: aSelector numLocals: numLocals stackSize: stackSize closureSize: closureSize [

	stream << 'compileMethod(' << aSelector << ',' << numLocals asString
	<< ',' << (stackSize asString , '+' , closureSize asString)
	<< ',.{'.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitPragma: pragmaNum [

	stream tab.
				stream << '&p.p' << pragmaNum asString
				<< ','.
				stream cr
]

{ #category : #accessing }
ASZigFileGenerator >> emitPushBlock: stackIndex [

	stream tab.
	stream << stackIndex asString.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitPushContext [
stream tab.
	stream << '&e.pushContext,"^",'.
	stream cr.
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitVerifySelector [
	stream tab.
			stream << '&e.verifySelector,'.
			stream cr 
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitZigConstants [
	stream << 'const std = @import("std");'.
	stream cr.
	stream << 'const debug = std.debug;'.
	stream cr.
	stream << 'const math = std.math;'.
	stream cr.
	stream << 'const stdout = std.io.getStdOut().writer();'.
	stream cr.
	stream << 'const Object = @import("zag/zobject.zig").Object;'.
	stream cr.
	stream << 'const Nil = @import("zag/zobject.zig").Nil;'.
	stream cr.
	stream
	<< 'const indexSymbol = $@import($"zag/zobject.zig$").indexSymbol;'.
	stream cr.
	stream << 'const execute = $@import($"zag/execute.zig$");'.
	stream cr.
	stream << 'const tailCall = execute.tailCall;'.
	stream cr.
	stream << 'const Code = execute.Code;'.
	stream cr.
	stream << 'const compileMethod = execute.compileMethod;'.
	stream cr.
	stream << 'const ContextPtr = execute.CodeContextPtr;'.
	stream cr.
	stream
	<<
	'const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;'.
	stream cr.
	stream << 'const TestExecution = execute.TestExecution;'.
	stream cr.
	stream << 'const primitives = @import("zag/primitives.zig");'.
	stream cr.
	stream << 'const Process = @import("zag/process.zig").Process;'.
	stream cr.
	stream << 'const symbol =  @import("zag/symbol.zig");'.
	stream cr.
	stream << 'const heap =  @import("zag/heap.zig");'.
	stream cr.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> emitZigSymbols: syms selectors: selectors [ 
	selectors removeAllFoundIn: syms keys.
	stream << 'const Sym = struct {'.
	stream cr.
	selectors do: [ :selector |
		stream tab.
		stream << selector asZigString << ': Object,'.
		stream cr ].
	stream tab.
	stream << 'const ss = heap.compileStrings(.{'.
	stream cr.
	selectors do: [ :aSelector |
		stream tab.
		stream tab.
		stream << '"' << aSelector << '",'.
		stream cr ].
	stream tab.
	stream << '});'.
	stream cr.
	stream tab.
	stream << 'usingnamespace symbol.symbols;'.
	stream cr.
	stream tab.
	stream << 'fn init() Sym {'.
	stream cr.
	stream tab.
	stream tab.
	stream << 'return .{'.
	stream cr.
	selectors withIndexDo: [ :aSelector :index |
		stream tab.
		stream tab.
		stream tab.
		stream << '.' << aSelector asZigString << ' = symbol.intern(ss['
		<< (index - 1) asString << '].asObject()),'.
		stream cr ].
	stream tab.
	stream tab.
	stream << '};'.
	stream cr.
	stream tab.
	stream << '}'.
	stream cr.
	stream << '};'.
	stream cr.
	stream cr
]

{ #category : #emmiting }
ASZigFileGenerator >> goTo: aLabel [ 
	"what does a go to look like?"
	instructions add: aLabel asLabel
]

{ #category : #accessing }
ASZigFileGenerator >> initialize [ 
	instructions := OrderedCollection new
]

{ #category : #initialization }
ASZigFileGenerator >> initializeZigSymbols [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"
	| syms |
	syms := ASZigSymbols new.
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
	syms
		at: 'value:value:value:value:'
		put: 'Sym.@"value:value:value:value:"'.
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
	syms at: 'perform:' put: 'Sym.@"perform:"'.
	syms at: 'perform:with:' put: 'Sym.@"perform:with:"'.
	syms at: 'perform:with:with:' put: 'Sym.@"perform:with:with:"'.
	syms
		at: 'perform:with:with:with:'
		put: 'Sym.@"perform:with:with:with:"'.
	syms
		at: 'perform:withArguments:'
		put: 'Sym.@"perform:withArguments:"'.
	syms
		at: 'perform:withArguments:inSuperclass:'
		put: 'Sym.@"perform:withArguments:inSuperclass:"'.
	syms at: '\\' put: 'Sym.@"\\"'.
	syms at: 'whileTrue:' put: 'Sym.@"whileTrue:"'.
	syms at: 'Object' put: 'Sym.Object'.
	^syms
]

{ #category : #accessing }
ASZigFileGenerator >> instructions [
	^ instructions
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushClose [
stream cr. "should this be hardcoded?"
	stream << '});'.
	stream cr.
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushDrop [
	instructions add: '&e.drop,'
]

{ #category : #accessing }
ASZigFileGenerator >> pushLabel: aLabel [

	instructions add: aLabel asLabel
]

{ #category : #instructions }
ASZigFileGenerator >> pushLiteral: aLiteral [

	instructions add: '&e.pushLiteral, ' , aLiteral asZagLiteral , ','
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushLocal: aSelector index: anIndex [

	instructions add: '&e.pushLocal, ' , anIndex asString , ','.
	self comment: aSelector asZigString
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushNamespace: namespace [

	stream cr.
	stream << 'var ' << namespace << ' ='.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushPopLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	stream tab.
				stream << '&e.popLocalData, '
				<<
				stackIndex
					asString << ' + (' << blockIndex
				<< ' << 8), // ' << name
				<< ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushReturn [
	instructions add: '&e.returnTop,'
]

{ #category : #'as yet unclassified' }
ASZigFileGenerator >> pushSendForSelector: aSelector numArgs: numArgs [

	instructions add:
		'&e.send' , numArgs asString , ', ' , aSelector , ','
]

{ #category : #accessing }
ASZigFileGenerator >> stream: aWriteStream [
	stream := aWriteStream
]

{ #category : #accessing }
ASZigFileGenerator >> symbol: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsent: [ name ]
]

{ #category : #accessing }
ASZigFileGenerator >> symbolOrVirtual: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsentPut: [ symbols virutalName ]
]
