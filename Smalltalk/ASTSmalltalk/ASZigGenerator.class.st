Class {
	#name : #ASZigGenerator,
	#superclass : #Object,
	#instVars : [
		'stream',
		'instructions',
		'symbols'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASZigGenerator class >> stream: aStream [
 ^ self new stream: aStream
]

{ #category : #accessing }
ASZigGenerator >> comment: aComment [

	instructions add: '// ' , aComment
]

{ #category : #accessing }
ASZigGenerator >> emitClosure: stackIndex modifier: size [
stream tab.
		stream << '&e.closureData, '
		<< stackIndex asString << ' + ('
		<< size asString
		<< ' << 8), // local:3 size:1 (offset 1 is l1)'.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitFooter: methods [

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
ASZigGenerator >> emitHeader: selectors [
	self emitZigConstants.
	symbols := self initialize.
	self emitZigSymbols: symbols selectors: selectors
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitInstruction: instruction [
stream tab.
		stream << instruction.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitMethod: aSelector numLocals: numLocals stackSize: stackSize closureSize: closureSize [

	stream << 'compileMethod(' << aSelector << ',' << numLocals asString
	<< ',' << (stackSize asString , '+' , closureSize asString)
	<< ',.{'.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitPragma: pragmaNum [

	stream tab.
				stream << '&p.p' << pragmaNum asString
				<< ','.
				stream cr
]

{ #category : #accessing }
ASZigGenerator >> emitPushBlock: stackIndex [

	stream tab.
	stream << stackIndex asString.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitPushContext [
stream tab.
	stream << '&e.pushContext,"^",'.
	stream cr.
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitVerifySelector [
	stream tab.
			stream << '&e.verifySelector,'.
			stream cr 
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> emitZigConstants [
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

{ #category : #emmiting }
ASZigGenerator >> goTo: aLabel [ 
	"what does a go to look like?"
	instructions add: aLabel asLabel
]

{ #category : #accessing }
ASZigGenerator >> initialize [ 
	instructions := OrderedCollection new
]

{ #category : #accessing }
ASZigGenerator >> instructions [
	^ instructions
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushClose [
stream cr. "should this be hardcoded?"
	stream << '});'.
	stream cr.
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushDrop [
	instructions add: '&e.drop,'
]

{ #category : #accessing }
ASZigGenerator >> pushLabel: aLabel [

	instructions add: aLabel asLabel
]

{ #category : #instructions }
ASZigGenerator >> pushLiteral: aLiteral [

	instructions add: '&e.pushLiteral, ' , aLiteral asZigLiteral , ','
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushLocal: aSelector index: anIndex [

	instructions add: '&e.pushLocal, ' , anIndex asString , ','.
	self comment: aSelector asZigString
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushNamespace: namespace [

	stream cr.
	stream << 'var ' << namespace << ' ='.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushPopLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	stream tab.
				stream << '&e.popLocalData, '
				<<
				stackIndex
					asString << ' + (' << blockIndex
				<< ' << 8), // ' << name
				<< ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushReturn [
	instructions add: '&e.returnTop,'
]

{ #category : #'as yet unclassified' }
ASZigGenerator >> pushSendForSelector: aSelector numArgs: numArgs [

	instructions add:
		'&e.send' , numArgs asString , ', ' , aSelector , ','
]

{ #category : #accessing }
ASZigGenerator >> stream: aWriteStream [
	stream := aWriteStream
]

{ #category : #accessing }
ASZigGenerator >> symbol: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsent: [ name ]
]

{ #category : #accessing }
ASZigGenerator >> symbolOrVirtual: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsentPut: [ symbols virutalName ]
]
