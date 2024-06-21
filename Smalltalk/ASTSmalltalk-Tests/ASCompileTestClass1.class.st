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
	#category : 'ASTSmalltalk-Tests-Code',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Code'
}

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
ASCompileTestClass1 >> bar [

	^ 42
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> block [

	^ { 
		  (3 + 4).
		  4 factorial } collect: [ :each | |temp| temp := each + 5. temp + 3 ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockReturn: p1 [
	true ifTrue: [ ^p1 ]
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> blockReturnEasy [

	true ifTrue: [ ^ 42 ]
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
ASCompileTestClass1 >> bracketSend [ 

	5 + (self fib: 2)
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

	self bar isLarge. "not"
	^ 42
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
ASCompileTestClass1 >> object [

^ Object new
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plus1: aNumber [
	^ aNumber + 1
]

{ #category : 'testing - examples' }
ASCompileTestClass1 >> plus: n1 and: n2 and: n3 [

	^ n1 + (n2 + n3)
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

{ #category : 'as yet unclassified' }
ASCompileTestClass1 >> recursiveMethod [
	^self recursiveMethod

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

{ #category : 'as yet unclassified' }
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
