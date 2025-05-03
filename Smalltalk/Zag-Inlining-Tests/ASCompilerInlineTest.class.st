Class {
	#name : 'ASCompilerInlineTest',
	#superclass : 'TestCase',
	#instVars : [
		'interpreter'
	],
	#category : 'Zag-Inlining-Tests-Tests',
	#package : 'Zag-Inlining-Tests',
	#tag : 'Tests'
}

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_fibonacci [
	" self <= 2 ifTrue: [ ^ 1 ]
 ^ (self - 1) fibonacci + (self - 2) fibonacci "

	| cg targets |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg locals: #(  ) parameters: #(  ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #'<='.
	cg pushNil.
	targets := cg
		           classCase: {
				           True.
				           False }
		           selector: #ifTrue:.
	cg setBasicBlock: targets first.
	cg drop.
	cg drop.
	cg pushLiteral: 1.
	cg returnTop.
	cg setBasicBlock: targets last.
	cg drop.
	cg drop.
	cg pushSelf.
	cg pushLiteral: 1.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg sendEmbedded: #+.
	cg returnTop.
	cg finalAnalysis.
	^ cg
]

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_fibonacci_alt [
	" ^ self <= 2 ifTrue: [ 1 ]
    ifFalse: [ (self - 1) fibonacci + (self - 2) fibonacci ]"

	| cg targets common |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #fibonacci.
	cg locals: #(  ) parameters: #(  ).
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #'<='.
	cg pushNil.
	common := cg newInline.
	targets := cg
		           classCase: {
				           True.
				           False }
		           selector: #ifTrue:.
	cg setBasicBlock: targets first.
	cg drop.
	cg pushLiteral: 1.
	cg branch: common.
	cg setBasicBlock: targets last.
	cg drop.
	cg pushSelf.
	cg pushLiteral: 1.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg pushSelf.
	cg pushLiteral: 2.
	cg sendEmbedded: #-.
	cg send: #fibonacci targetClass: nil.
	cg sendEmbedded: #+.
	cg branch: common.
	cg setBasicBlock: common.
	cg returnTop.
	cg finalAnalysis.
	^ cg
]

{ #category : 'hand compiled' }
ASCompilerInlineTest class >> inlined_insertionSorted [
	" Array>>#size, Array>>at:, Array>>at:put: and BlockClosure>>#value are primitive

defined in SequencableCollection	
	insertionSorted
	2 to: self size do: [ :i | self insertElementAt: i ]
	
	insertElementAt: i
	| new |
	new := self at: i.
	i to: 2 by: -1 do: [ :j |
		| old |
		old := self at: j - 1.
		new >= old ifTrue: [
			self at: j put: new.
			^ self ].
		self at: j put: old ].
	self at: 1 put: new
	
defined in Number
	to: stop do: aBlock
	| nextValue |
	nextValue := self.
	[ nextValue <= stop ] whileTrue: [
		aBlock value: nextValue.
		nextValue := nextValue + 1 ]

defined in BlockClosure
	whileTrue: aBlock
	self value ifFalse: [ ^ nil ].
	aBlock value.
	^ self whileTrue: aBlock
	"

	| cg |
	cg := ASCodeGenerator new.
	cg targetClass: Array.
	cg selector: #insertionSorted.
	cg locals: #(  ) parameters: #(  ).
	cg pushLiteral: 2.
	cg pushSelf.

	^ cg
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assert: aMethod inlinesTo: ast [

	| result |
	result := ASPharoCompiler new compileMethod: aMethod.
	"result inlineMethod."
	self assert: result equals: ast
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assertEquivalentSending: selecter arguments: aCollection to: anObject [
	self
		assert: (interpreter send: selecter with: aCollection to: anObject)
		equals: (anObject perform: selecter withArguments: aCollection).
		interpreter inspect.
	self assert: interpreter stackDepth equals: 1
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assertEquivalentSending: selecter arguments: aCollection to: anObject inlineDepth: inlineDepth [
	interpreter inlineDepth: inlineDepth.
 	self
		assert: (interpreter send: selecter with: aCollection to: anObject)
		equals: (anObject perform: selecter withArguments: aCollection).
		interpreter inspect.
	self assert: interpreter stackDepth equals: 1
]

{ #category : 'asserting' }
ASCompilerInlineTest >> assertEquivalentSending: selecter to: anObject [

	self assertEquivalentSending: selecter arguments: #() to: anObject 
]

{ #category : 'asserting' }
ASCompilerInlineTest >> class: aClass selector: aSelector [
	aClass ifNil: [ ^nil ].
	aClass methodDictionary at: aSelector ifPresent: [ ^ aClass >> aSelector ].
	^ self class: aClass superclass selector: aSelector
]

{ #category : 'running' }
ASCompilerInlineTest >> setUp [

	super setUp.

	ASInliner reset.
	interpreter := ASemanticInterpreter new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testArray [
	"array

	^ { 17. 42. 99 }"

	self assertEquivalentSending: #array to: ASCompileTestClass1 new.
	interpreter inspect.
	self assert: interpreter basicBlocks size equals: 1.
	self assert: interpreter codeSize equals: 1
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBar [
	"
bar

	^ 42"

	self assertEquivalentSending: #bar to: ASCompileTestClass1 new.
	self assert: interpreter basicBlocks size equals: 1.
	interpreter inspect.
	self
		assert:
		interpreter codeSize
		equals: 1
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBarInlined [
	"bar

	^ 42"

	interpreter inlineDepth: 100.
	self testBar.
	self assert: interpreter basicBlocks size equals: 1
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationNestedWithSend [
	"blockEvaluationNestedWithSend
	^ [ | x | x := [ 10 + 5 ] value. x ] value"
	
	self assertEquivalentSending: #blockEvaluationNestedWithSend to: ASCompileTestClass1 new.
	
	
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationWithCall [
	"blockEvaluationWithCall
		""bar ^42""
		^ [ self bar + 5 ] value"
	
	self assertEquivalentSending: #blockEvaluationWithCall to: ASCompileTestClass1 new.
	
	
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationWithCallAndLocalWithoutTailCall [
	"blockEvaluationWithCallAndLocalWithoutTailCall
		""bar ^42""
		| x |
		x := [ self bar + 5 ] value.
		^ x "

	self
		assertEquivalentSending: #blockEvaluationWithCallAndLocalWithoutTailCall
		to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationWithCallWithoutTailCall [
	"blockEvaluationWithCallWithoutTailCall
		""bar ^42""
		[ self bar + 5 ] value"

	self
		assertEquivalentSending: #blockEvaluationWithCallWithoutTailCall
		to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationWithSend2 [
	"blockEvaluationWithSend2
	^ [ 10 + 5 ] value"
	
	self assertEquivalentSending: #blockEvaluationWithSend2 to: ASCompileTestClass1 new.
	
	
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockEvaluationWithSendWithoutTailCall [
	"blockEvaluationWithSendWithoutTailCall
	| x |
	x := [ 10 + 5 ] value.
	^ x"
	
	self assertEquivalentSending: #blockEvaluationWithSendWithoutTailCall to: ASCompileTestClass1 new.
	
	
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockReturnEasyInline [
	"
blockReturnEasy

	true ifTrue: [ ^ 42 ] ifFalse: [ ^ self ]
"

"	(ASNoOutput withInlining compileMethod:
		 ASCompileTestClass1 >> #blockReturnEasy) inspect"
		
	self assertEquivalentSending: #blockReturnEasy to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockReturnInline [
	"
blockReturn: p1
	true ifTrue: [ ^ p1 ]
"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturn:.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockValue [
	"blockValue

	^ [ 4 ] value "

	self assertEquivalentSending: #blockValue to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockValueInlined [
	"blockValue

	^ [ 4 ] value "

	interpreter inlineDepth: 1.
	self testBlockValue
]

{ #category : 'tests' }
ASCompilerInlineTest >> testBlockValueNonlocal [
	"blockValue

	^ [ 4 ] value "

	self assertEquivalentSending: #blockValueNonlocal to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibWith3 [

"fib: anInt

	^ anInt < 2
		  ifTrue: [ 1 ]
		  ifFalse: [ (self fib: anInt - 1) + (self fib: anInt - 2) ]"
	self
		assertEquivalentSending: #fib:
		arguments: #( 3 )
		to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacci [
	"
fibonacci

	self < 2 ifTrue: [ ^ self ].
	^ (self - 1) fibonacci + (self - 2) fibonacci"

	self assertEquivalentSending: #fibonacci to: 4
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacciFast [
	"
fibonacci_fast

	^ self fibonacci_accumulator: 1 prev: 0
	
fibonacci_accumulator: accumulator prev: prev

	self = 0 ifTrue: [ ^ prev ].
	^ self - 1
		  fibonacci_accumulator: prev + accumulator
		  prev: accumulator"

	self assertEquivalentSending: #fibonacci_fast to: 4
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacciFastInline [
	"
fibonacci_fast

	^ self fibonacci_accumulator: 1 prev: 0
	
fibonacci_accumulator: accumulator prev: prev

	self = 0 ifTrue: [ ^ prev ].
	^ self - 1
		  fibonacci_accumulator: prev + accumulator
		  prev: accumulator"

	interpreter inlineDepth: 1.
	self testFibonacciFast
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFibonacci_fast_forCorrectness [

	self assert: 0 fibonacci_fast equals: 0 fibonacci.
	self assert: 1 fibonacci_fast equals: 1 fibonacci.
	self assert: 2 fibonacci_fast equals: 2 fibonacci.
	self assert: 3 fibonacci_fast equals: 3 fibonacci.
	self assert: 4 fibonacci_fast equals: 4 fibonacci
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo [
	"foo
	^ self bar"

	"bar
	^ 42"
	self assertEquivalentSending: #foo to: ASCompileTestClass1 new.
	self assert: interpreter codeSize equals: 2.
	interpreter inspect.
	self
		assert:
		(interpreter dispatchFor: ASCompileTestClass1 >> #foo) codeSize
		equals: 3
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo1 [
	"foo
	^ self bar isLarge"

	"bar
	^ 42"

	self assertEquivalentSending: #foo1 to: ASCompileTestClass1 new

]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo1Inlined [
	"foo
	^ self bar isLarge"

	"bar
	^ 42"

"	interpreter inlineDepth: 1.
	self testFoo1"
	self assertEquivalentSending: #foo arguments: { } to: ASCompileTestClass1 new inlineDepth: 1000.
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo2Inlined [
	"foo
	^ self bar"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo2.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFoo3Inlined [
	"foo3
	self bar"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo3.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testFooInlined [
	"foo
	self bar isLarge.
	^ 42"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator targetClass: ASCompileTestClass1 selector: #foo.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testGives43 [

	self assertEquivalentSending: #gives43 to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testInlinePlus1Inlined [
	"inlinePlus1
	^ self plus1: 30"

	"plus1: aNumber
	^ aNumber + 1"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #inlinePlus1.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testInsertionSort [

	self
		assert: #( 1 2 3 4 5 6 ) copy insertionSorted
		equals: #( 1 2 3 4 5 6 ).
	self
		assert: #( 6 2 4 3 5 1 ) copy insertionSorted
		equals: #( 1 2 3 4 5 6 )
]

{ #category : 'tests' }
ASCompilerInlineTest >> testNegatedInlined [
	"inlinePlus1
	^ self plus1: 30"

	"plus1: aNumber
	^ aNumber + 1"

	| cg |
	cg := ASCodeGenerator targetClass: SmallInteger selector: #negated.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASCompilerInlineTest >> testPlus1 [
	"plus1: aNumber
	^ aNumber + 1"

	self assertEquivalentSending: #plus1: arguments: #(6) to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testPrimitive [

	self assertEquivalentSending: #testDiv: arguments: #( 2 ) to: 6
]

{ #category : 'tests' }
ASCompilerInlineTest >> testPrimitiveFailing [

	self assertEquivalentSending: #testDiv: arguments: #( 0 ) to: 6
]

{ #category : 'tests' }
ASCompilerInlineTest >> testRecursiveSum [
	"recursiveSum: listOfNumbers
		listOfNumbers empty ifTrue: [^0].
		^ listOfNumbers first + listOfNumbers allButFirst"

	self assertEquivalentSending: #recursiveSum: arguments: #(#(4 5 6)) to: ASCompileTestClass1 new
]

{ #category : 'tests' }
ASCompilerInlineTest >> testWhileTrueInlined [
	"whileTrue

	| dirty |
	[
	dirty := false.
	dirty ] whileTrue"

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #whileTrue) inspect
]

{ #category : 'hand compiled' }
ASCompilerInlineTest >> test_inlined_insertionSorted [

	| cg |
	cg := self class inlined_insertionSorted.
	cg inspect
]
