"
I contain methods that are used as exemplars for tests.
"
Class {
	#name : 'ASCompileTestClass1',
	#superclass : 'Object',
	#instVars : [
		'bar'
	],
	#classVars : [
		'Foo'
	],
	#category : 'Zag-Compiler-Tests-Code',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Code'
}

{ #category : 'testing - examples' }
ASCompileTestClass1 class >> classVar: anObject [

	Foo := anObject
]

{ #category : 'starting' }
ASCompileTestClass1 class >> start42 [

	^ 42 negated
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> + n1 [
	| temp |
	temp := self + n1.
	^ temp
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> addTwoNumbers [

	| sum foo |
	foo := sum := 4 + 5.
	^ sum + foo
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> array [

	^ { 17. 42. 99 }
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> bar [

	^ 42
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> bar: aBlock [

	^ aBlock value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> barCallsBlat [

	self blat: 50
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blat: aNumber [

	(aNumber < 0) ifTrue: [  aNumber ] ifFalse: [ aNumber negated ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> block [

	^ { 
		  (3 + 4).
		  4 factorial } collect: [ :each | |temp| temp := each + 5. temp + 3 ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationNestedWithSend [
	^ [ | x | x := [ 10 + 5 ] value. x ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationWithCall [
"bar ^42"
	^ [ self bar + 5 ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationWithCallAndLocalWithoutTailCall [
	"bar ^42"

	| x |
	x := [ self bar + 5 ] value.
	^ x
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationWithCallWithoutTailCall [
	"bar ^42"

	[ self bar + 5 ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationWithSend2 [

	^ [ 10 + 5 ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockEvaluationWithSendWithoutTailCall [
	| x |
	x := [ 10 + 5 ] value.
	^ x
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockReturn: p1 [
	true ifTrue: [ ^p1 ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockReturnEasy [

	true ifTrue: [ ^ 42 ] ifFalse: [ ^ self ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockReturnSelf [
	true ifTrue: [ ^self ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockValue [

	^ [ 4 ] value 
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockValueNonlocal [

	[ ^ 4 ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockValueWithBlock [

	^ [ [ 4 ] value ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockWithAdd [

	^ [ 5 +10 ] value
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> bracketSend [ 

	5 + (self fib: 2)
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> bracketWithValue [
	^ [ :x | x + 5 ] value: 10
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> cascade [

	| foo |
	foo := #( abc nil 3 ) asOrderedCollection.
	(foo
		 add: 4 negated;
		 yourself)
		add: 3 negated + 4 abs;
		size;
		negated;
		+ 4;
		+ 3 negated;
		abs
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> classVar [

	^ Foo
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> collectionAnyOne [
| someArray |
	someArray = #(5 6 2 10 88).
	^ someArray anyOne
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> converter: param [

	| local |
	
	local := 10.
	
	....
	
	local negated.
	
	
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> counter [

	| aBlock counter |
	counter := 1.


	"Block reads and writes to counter"
	aBlock := [
		          counter counter + 1.
		          ^ counter ].

	Transcript
		show: counter;
		cr. "Prints 1"

	aBlock value Transcript
		show: counter;
		cr "Prints 2"
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fib2: anInt [

	anInt < 2 ifTrue: [ ^ 1 ].
	^ (self fib2: anInt - 1) + self fib2: anInt - 2
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fib: anInt [
	^ anInt < 2
		ifTrue: [1] 
		ifFalse: [(self fib: anInt-1) + (self fib: anInt-2)]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fibonacci [

	^ self < 2 ifTrue: [ 1 ] ifFalse: [(self - 1) fibonacci + (self - 2) fibonacci]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fibonacci_fast [

	^ 23 fibonacci_fast
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> foo [

	^ self bar
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> foo1 [

	^ self bar isLarge
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> foo2 [

	^ self bar
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> foo3 [

	self bar
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> foo: p1 bar: p2 [
    | l1 l2 l3 |
    p1 < p2 ifTrue: [ ^ self ].
    l1 := p2.
    l2 := p1 \\ p2.
    l3 := p2 - l2.
    [ l1 < p1 ] whileTrue: [
        l1 := l1 + 1.
        l1 = l3 ifTrue: [ ^ 1 ] ].
    ^ l1
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fooBarBlock [

	^ self bar: [ 42 ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> fooCallsBarCallsBlat [

	self barCallsBlat
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> gives43 [
	| forty2 |
	forty2 := self unaries * 2.
	^ self plus1: (forty2 - 4)
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> inlinePlus1 [
	^ self plus1: 30
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> instVar [

	^ bar
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> localVarAccessing [

	| forty2 |
	forty2 := 34.
	^ forty2 negated
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> object [

^ Object new
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plus1: aNumber [
	^ aNumber + 1
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plus: n1 and: n2 [

	^ n1 + (42 + n2)
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plus: n1 and: n2 and: n3 [

	^ 5 + (n1 + (n2 + n3)) + 4
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plusWithLocal: n1 and: n2 and: n3 [
| sum |
	sum := n1 + n2 + n3.
	^sum
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> prim [
	<primitive: 1>
	^ false
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> recursiveMethod [

	self recursiveMethod.
	^ self
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> recursiveSum: listOfNumbers [
	listOfNumbers empty ifTrue: [^0].
	^ listOfNumbers first + listOfNumbers allButFirst
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simple1 [
	..
	^ 42..
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simple1_noreturn [

	 42
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simple2 [

	^ 25 + 17
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simple2_noreturn [

	25 + 17
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simpleBlock [

	^ [ :each |
	  | temp |
	  temp := each + 5.
	  temp + 3 ] value: 42
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simplePopExample [
	^ 35
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> simplePushPopExample [
	| x |
	x := 35.
	^x
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> tailRecursiveMethod [

	^ self tailRecursiveMethod
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> testAssignment [

	| a1 a2 a3 a4 |
	a1 := 5.
	a2 := 5 negated.
	a3 := self.
	a4 := self prim.
	^ a1 + a2
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> unaries [
	^ 23 negated abs
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> unariesPlus [

	42 negated. 
	^ 23 negated abs max: 42
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> whileTrue [

	[ false ] whileTrue
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> whileTrueHarder [

	| dirty |
	[
	dirty := false.
	dirty ] whileTrue
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> | n1 [
	| temp |
	temp := self | n1.
	^ temp
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> || n1 [
	| temp temp2 |
	temp := temp2 :=self || n1.
	^ temp
]
