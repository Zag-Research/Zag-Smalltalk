Class {
	#name : #ASPharoCompilerProgramTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASPharoCompilerProgramTest class >> testFooBarIsolated [

	<script>
	| ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #foo:bar:.
	file := ASZigFileContext new.

	ASZigThreadedOutput new
		context: file;
		generateCode: ast.

	FileSystem disk workingDirectory / 'zigFooBarCompileMethod.zig'
		writeStreamDo: [ :stream | file outputTo: stream ] 
]

{ #category : #compiling }
ASPharoCompilerProgramTest >> buildFromClass: class selector: method [

	^ ASPharoCompiler new buildFromClass: class selector: method
]

{ #category : #testing }
ASPharoCompilerProgramTest >> buildFromClassShallow: class selector: method [

	^ ASPharoCompiler new
		  buildFromClassShallow: class
		  selector: method
]

{ #category : #testing }
ASPharoCompilerProgramTest >> buildFromMethods: methods [

	| compiler |
	compiler := ASPharoCompiler new.
	compiler dontVisitMessages.
	methods do: [ :method | compiler addMethod: method ].
	^ compiler transitiveClosure
]

{ #category : #script }
ASPharoCompilerProgramTest >> codeStart42 [
	<script>
	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	^ result zigString
]

{ #category : #testing }
ASPharoCompilerProgramTest >> compileMethod: method [

	| compiler |
	compiler := ASPharoCompiler new.
	compiler dontVisitMessages.
	compiler addMethod: method.
	^ compiler transitiveClosure
]

{ #category : #testing }
ASPharoCompilerProgramTest >> compileMethodWithInlining: method [

	| compiler |
	compiler := ASPharoCompiler new.
	compiler addMethod: method.
	compiler transitiveClosure.
	^ compiler inline
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testAddingTwoNumbers [

	| result aStream |
	result := self compileMethodWithInlining:
		          ASCompileTestClass1 >> #addTwoNumbers.

	aStream := (FileSystem disk workingDirectory
	            / 'zigAddingTwoNumbers.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testAssignment [

	| result aStream |
	result := self buildFromClassShallow: ASCompileTestClass1 selector: #testAssignment.
	
	aStream := (FileSystem disk workingDirectory
	            / 'zigAssignmentString.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #script }
ASPharoCompilerProgramTest >> testCodeStart42 [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	
	(FileSystem disk workingDirectory / 'zigString.txt') writeStream
	nextPutAll: result zigString;
	close.
	
	self assert: result zigString equals: ''
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFib [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #foo:bar:.

	(FileSystem disk workingDirectory / 'zigFooBarString.txt') writeStream
		nextPutAll: result zigString;
		close.

	self assert: result zigString equals: ''
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFibSubset [

	| result methods aStream |
	methods := OrderedCollection new.
	methods add: ASCompileTestClass1 >> #fib:.
	methods add: SmallInteger >> #+.
	methods add: SmallInteger >> #-.
	methods add: SmallInteger >> #<=.
	methods add: True >> #ifTrue:.
	methods add: False >> #ifTrue:.
	result := self buildFromMethods: methods.

	aStream := (FileSystem disk workingDirectory / 'zigFibSubset.zig')
		           writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFooBar [

	| result aStream |
	result := self buildFromClass: ASCompileTestClass1 selector: #foo:bar:.

	aStream := (FileSystem disk workingDirectory / 'zigFooBarString.zig')
		           writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFooBarInlining [

	| result aStream |
	result := self compileMethodWithInlining:
		          ASCompileTestClass1 >> #foo:bar:.

	aStream := (FileSystem disk workingDirectory
	            / 'zigFooBarInlining.zig') writeStream.
	aStream truncate.
		self halt.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFooBarIsolated [
	^ self class testFooBarIsolated
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFooBarIsolatedNew [

	| compiler codeGenerator |
	compiler := ASPharoCompiler
		            buildMethod: ASCompileTestClass1 >> #foo:bar:
		            forClass: ASCompileTestClass1
		            withCodeGenerator: (ASZigGenerator stream:
				             (FileSystem disk workingDirectory
				              / 'zigFooBarStringNew.zig') writeStream).
	compiler compile.
	self halt
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFooBarSubset [

	| result aStream |
	result := self compileMethod: ASCompileTestClass1 >> #foo:bar:.

	aStream := (FileSystem disk workingDirectory / 'zigFooBarSubset.zig')
		           writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntFib [

	| result aStream |
	result := self buildFromClass: Integer selector: #fibonacci.

	aStream := (FileSystem disk workingDirectory / 'zigIntFibString.zig')
		           writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntFibIsolated [

	| result aStream |
	result := self buildFromClassShallow: Integer selector: #fibonacci.

	aStream := (FileSystem disk workingDirectory
	            / 'zigIntFibIsolatedString.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntbitShiftMagnitude [
	| result aStream |
	result := self
		          buildFromClass: Integer class
		          selector: #bitShiftMagnitude:.
	

	aStream := (FileSystem disk workingDirectory / 'zigBitShiftMagnitudeString.zig') writeStream.
	result writeZig: aStream.
	aStream close.
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntegerBench [

	| result aStream |
	result := self
		          buildFromClass: Integer
		          selector: #benchmark.
	aStream := (FileSystem disk workingDirectory
	            / 'zigIntBenchString.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testNestedBlocks [

	| result aStream |
	result := self compileMethodWithInlining:
		          ASCompileTestClass1 >> #nestedBlock.

	aStream := (FileSystem disk workingDirectory
	            / 'zigAddingTwoNumbers.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testPlusWithLocal [

	| result aStream |
	result := self
		          buildFromClass: ASCompileTestClass1
		          selector: #plusWithLocal:and:and:.
	aStream := (FileSystem disk workingDirectory
	            / 'zigPlusWithLocalString.zig') writeStream.
	aStream truncate.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testStart42 [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	self assert: result classes equals: { 
			ASTObject.
			ASTBehavior.
			ASTMetaclass .
			Metaclass.
			ASTClass.
			ASTClassDescription.
			ASTSmallInteger.
			ASTInteger.
			ASTNumber.
			ASTMagnitude.
			ASTTrue.
			ASTFalse.
			ASTBoolean.
			ASTBlockClosure.
			ASTCharacter.
			ASTString.
			ASTArray.
			ASTArrayedCollection.
			ASTSequenceableCollection.
			ASTCollection.
			ASCompileTestClass1 } asSet.
	self
		assert: result selectors
		equals: #( at:put: do: negated bitOr: + perform:withArguments:
			   numArgsError: minVal byteAt: new:neg: new: new or: digitCompare:
			   igitLogic:op:length: copyReplaceFrom:to:with:
			   > cull: superclass primitiveFailed isKindOf: basicNew whileFalse:
			   and: perform:with: at: copyto: digitSubtract: error: basicNew:
			   * digitDiv:neg: start42 & isEmpty digitRshift:bytes:lookfirst:
			   adaptToInteger:andSend: ~~ errorSubscriptBounds:
			   numArgs normalize instSpec min: errorImproperStore
			   negative isInteger quo: highBit basicAt: = species asInteger
			   first whileTrue: value bytesCount digitAdd: cannotInstantiate
			   // ifNil:ifNotNil: asCharacter \\ emptyCheck errorEmptyCollection
			   value: - replaceFrom:to:with:startingAt:
			   ifFalse: errorNonIntegerIndex shouldNotImplement
			   bitShift: growby: floor subclassResponsibility
			   bitAnd: isSmallInteger class add: < with: compare:with:collated:
			   >= size <= growto: max: not == , ifTrue: bitInvert ifTrue:ifFalse:
			   printOn: printString digitLshift: inheritsFrom: isVariable
			   digitMultiply:neg: byteAt:put: abs basicSize with:with:
			   bitClear: basicAt:put: initialize last to:do: isNumber
			   ~= truncated name lastDigit / ) asSet.
	result inspect
]
