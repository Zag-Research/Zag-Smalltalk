Class {
	#name : #ASZigCompileMethodGenerator,
	#superclass : #ASZigCodeGenerator,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASZigCompileMethodGenerator class >> stream: aStream [
 ^ self new stream: aStream
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> emitClosure: stackIndex modifier: size [
stream tab.
		stream << '&e.closureData, '
		<< stackIndex asString << ' + ('
		<< size asString
		<< ' << 8), // local:3 size:1 (offset 1 is l1)'.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> emitFooter: methods [

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
"	methods do: [ :aMethod | aMethod writeLiterals: stream with: symbols ].
	methods do: [ :aMethod | aMethod writeDispatch: stream with: symbols ].
"
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
ASZigCompileMethodGenerator >> emitInstruction: instruction [
stream tab.
		stream << instruction.
		stream cr
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> emitMethod: aSelector numLocals: numLocals stackSize: stackSize closureSize: closureSize [

	stream << 'compileMethod(' << aSelector << ',' << numLocals asString
	<< ',' << (stackSize asString , '+' , closureSize asString)
	<< ',.{'.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> emitPragma: pragmaNum [

	stream tab.
				stream << '&p.p' << pragmaNum asString
				<< ','.
				stream cr
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> emitPushBlock: stackIndex [

	stream tab.
	stream << stackIndex asString.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> emitPushContext [
stream tab.
	stream << '&e.pushContext,"^",'.
	stream cr.
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> emitVerifySelector [
	stream tab.
			stream << '&e.verifySelector,'.
			stream cr 
]

{ #category : #emmiting }
ASZigCompileMethodGenerator >> goTo: aLabel [
	"what does a go to look like?"

	self labelRef: aLabel
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushDrop [
	instructions add: '&e.drop,'
]

{ #category : #accessing }
ASZigCompileMethodGenerator >> pushLabel: aLabel [

	instructions add: aLabel asLabel
]

{ #category : #instructions }
ASZigCompileMethodGenerator >> pushLiteral: aLiteral [

	instructions add: '&e.pushLiteral, ' , aLiteral asZigLiteral , ','
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushLocal: aSelector index: anIndex [

	instructions add: '&e.pushLocal, ' , anIndex asString , ','.
	self comment: aSelector asZigString
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushNamespace: namespace [

	stream cr.
	stream << 'var ' << namespace << ' ='.
	stream cr
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushPopLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	stream tab.
				stream << '&e.popLocalData, '
				<<
				stackIndex
					asString << ' + (' << blockIndex
				<< ' << 8), // ' << name
				<< ' (read-only) copy offset 3 in local 1 (field in BC1)'
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushReturn [
	instructions add: '&e.returnTop,'
]

{ #category : #'as yet unclassified' }
ASZigCompileMethodGenerator >> pushSendForSelector: aSelector [

	self nextPutAll: 
		'&e.send' , aSelector numArgs asString , ', ' , aSelector , ','
]
