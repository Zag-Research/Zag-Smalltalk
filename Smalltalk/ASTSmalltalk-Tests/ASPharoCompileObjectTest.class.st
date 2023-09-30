Class {
	#name : #ASPharoCompileObjectTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASPharoCompileObjectTest class >> writeCompileMethod_fib [

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
ASPharoCompileObjectTest >> assert: anObject equalsZig: match [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	anObject asZig: codeGen.
	self
		assert: codeGen asString
		equals: match
]

{ #category : #tests }
ASPharoCompileObjectTest >> assert: anObject equalsZigAlt: match [

	| file codeGen |
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	anObject asZig: codeGen.
	self assert: codeGen setLiteralString equals: match
]

{ #category : #tests }
ASPharoCompileObjectTest >> testCompileMethodSmallIntegerMinus [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree |
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #-.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
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
ASPharoCompileObjectTest >> testCompileMethodToObject [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree |
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #-.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
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
ASPharoCompileObjectTest >> testCompileMethod_SmallIntegerPlus [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree |
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #+.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
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
ASPharoCompileObjectTest >> testCompileMethod_fib [

	| ast codeGen file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	codeGen := ASZigCompileObjectGenerator new context: file.
	codeGen generateCodeFor: ast.
	self assert: codeGen size equals: 10.
	self assert: file symbolNumber equals: 2.
	self assert: file classNumber equals: 0
]

{ #category : #tests }
ASPharoCompileObjectTest >> testCompileMethod_foobar [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream aReader lines astTree |
	ASStatement resetIndex.
	compiler := ASPharoCompiler new
		            buildFromClass: ASCompileTestClass1
		            method: ASCompileTestClass1 >> #foo:bar:.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
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
ASPharoCompileObjectTest >> testSimpleObjects [

	self assert: 42 equalsZig: '42'.
	self assert: true equalsZig: 'True'.
	self assert: false equalsZig: 'False'.
	self assert: nil equalsZig: 'Nil'.
	self assert: #value equalsZig: 'Sym.value'.
	self assert: #value: equalsZig: 'Sym.@"value:"'.
	self assert: #foo equalsZig: 'Sym.i_1'.
	self assert: #foo equalsZigAlt: 'sym.foo'.
	self assert: #foo:bar: equalsZig: 'Sym.i_1'.
	self assert: #foo:bar: equalsZigAlt: 'sym.@"foo:bar:"'.

]

{ #category : #tests }
ASPharoCompileObjectTest >> testVarObjects [

	self assert: 3 @ 4 equalsZig: '3,4,":L1",c.Point,'.
	self
		assert: false -> true
		equalsZig: 'False,True,":L1",c.Association,'.
	self
		assert: 3 @ true -> (false -> (#abc -> nil))
		equalsZig: '3,True,":L1",c.Point,Sym.i_1,Nil,":L2",c.Association,False,"L2",":L3",c.Association,"L1","L3",":L4",c.Association,'
]
