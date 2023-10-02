Class {
	#name : #ASZigCompileObjectTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCompileObjectTest class >> testCompileMethodSmallIntegerMinus [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree anASPharoCompileObjectTest |
	anASPharoCompileObjectTest := self new.
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #-.
	result := compiler asCompileObject.
	anASPharoCompileObjectTest assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerMinusAsCompileObject.zig') writeStream.
	aStream truncate.
	result do: [ :line |
		aStream << line.
		aStream cr ].
	aStream close.

	aReader := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') readStream.
	lines := OrderedCollection new.
	[ aReader atEnd ] whileFalse: [ lines add: aReader nextLine ].
	aReader close.

	astTree := compiler asAST: lines
]

{ #category : #tests }
ASZigCompileObjectTest class >> testCompileMethodToObject [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree anASPharoCompileObjectTest |
	anASPharoCompileObjectTest := self new.
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #-.
	result := compiler asCompileObject.
	anASPharoCompileObjectTest assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerMinusAsCompileObject.zig') writeStream.
	aStream truncate.
	result do: [ :line |
		aStream << line.
		aStream cr ].
	aStream close.

	aReader := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') readStream.
	lines := OrderedCollection new.
	[ aReader atEnd ] whileFalse: [ lines add: aReader nextLine ].
	aReader close.

	astTree := compiler asAST: lines
]

{ #category : #tests }
ASZigCompileObjectTest class >> testCompileMethod_SmallIntegerPlus [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree anASPharoCompileObjectTest |
	anASPharoCompileObjectTest := self new.
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #+.
	result := compiler asCompileObject.
	anASPharoCompileObjectTest assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') writeStream.
	aStream truncate.
	result do: [ :line |
		aStream << line.
		aStream cr ].
	aStream close.

	aReader := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') readStream.
	lines := OrderedCollection new.
	[ aReader atEnd ] whileFalse: [ lines add: aReader nextLine ].
	aReader close.

	astTree := compiler asAST: lines
]

{ #category : #tests }
ASZigCompileObjectTest class >> testCompileMethod_foobar [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree anASPharoCompileObjectTest |
	anASPharoCompileObjectTest := self new.
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: ASCompileTestClass1
		            method: ASCompileTestClass1 >> #foo:bar:.
	result := compiler asCompileObject.
	anASPharoCompileObjectTest assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigFooBarAsCompileObject.zig') writeStream.
	aStream truncate.
	result do: [ :line |
		aStream << line.
		aStream cr ].
	aStream close.

	aReader := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') readStream.
	lines := OrderedCollection new.
	[ aReader atEnd ] whileFalse: [ lines add: aReader nextLine ].
	aReader close.

	astTree := compiler asAST: lines
]

{ #category : #tests }
ASZigCompileObjectTest class >> writeCompileMethod_fib [

	<script>
	| ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	ASZigCompileObjectGenerator new
		context: file;
		generateCodeFor: ast.
	FileSystem disk workingDirectory / 'zigFibAsCompileObject.zig'
		writeStreamDo: [ :stream | file outputTo: stream ]
]

{ #category : #tests }
ASZigCompileObjectTest >> testCompileMethod_fib [

	| ast codeGen file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	codeGen generateCodeFor: ast.
	self assert: codeGen name equals: 'Obj1'.
	self
		assert: codeGen asString
		equals:
		'Sym.i_1,":L1",c.ASArg,"L1",":L2",c.ASRef,2,":L3",c.ASLiteral,"L3",":L4",c.Array,"L2",Sym.@"<","L4",":L5",c.ASSend,":L6",c.Array,"L6",Nil,":L7",c.ASSequence,":L8",c.Array,":L9",c.Array,"L7","L8","L9",":L10",c.ASBlock,":L11",c.Array,"L11",Nil,":L12",c.ASSequence,":L13",c.Array,":L14",c.Array,"L12","L13","L14",":L15",c.ASBlock,"L10","L15",":L16",c.Array,"L5",Sym.i_2,"L16",":L17",c.ASSend,"L17",":L18",c.ASReturn,"L18",":L19",c.Array,"L19",Nil,":L20",c.ASSequence,":L21",c.Array,Sym.i_1,":L22",c.ASArg,"L22",":L23",c.Array,":L24",c.Array,"L20","L21","L23",Sym.i_3,Nil,"L24",":L25",c.ASMethodNode,'
]

{ #category : #tests }
ASZigCompileObjectTest >> testSimpleObjects [

	self assert: 42 zigCGString equals: '42'.
	self assert: true zigCGString equals: 'True'.
	self assert: false zigCGString equals: 'False'.
	self assert: nil zigCGString equals: 'Nil'.
	self assert: #value zigCGString equals: 'Sym.value'.
	self assert: #value: zigCGString equals: 'Sym.@"value:"'.
	self assert: #foo zigCGString equals: 'Sym.i_1'.
	self assert: #foo zigSLString equals: 'sym.foo'.
	self assert: #foo:bar: zigCGString equals: 'Sym.i_1'.
	self assert: #foo:bar: zigSLString equals: 'sym.@"foo:bar:"'
]

{ #category : #tests }
ASZigCompileObjectTest >> testVarObjects [

	self assert: (3 @ 4) zigCGString equals: '3,4,":L1",c.Point,'.
	self
		assert: (false -> true) zigCGString
		equals: 'False,True,":L1",c.Association,'.
	self
		assert: (3 @ true -> (false -> (#abc -> nil))) zigCGString
		equals:
		'3,True,":L1",c.Point,Sym.i_1,Nil,":L2",c.Association,False,"L2",":L3",c.Association,"L1","L3",":L4",c.Association,'
]
