Class {
	#name : 'ZagSmalltalkTest',
	#superclass : 'TestCase',
	#instVars : [
		'method'
	],
	#category : 'Zag-Compiler-Tests-Tests',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Tests'
}

{ #category : 'asserting' }
ZagSmalltalkTest >> assertEquivalent: anObject [ 
	| result |
	result := ZagSmalltalk new statementForTest: anObject printString.
	self assert: result baseValue equals: anObject.

]

{ #category : 'asserting' }
ZagSmalltalkTest >> expectError: aString [

	self should: [ ZagSmalltalk new statementForTest: aString ] raise: Error
]

{ #category : 'tests' }
ZagSmalltalkTest >> methodFrom: aString [

	method := ZagSmalltalk new
		  compileMethod: aString
		  inClass: (ASClassNode new from: self class)
]

{ #category : 'asserting' }
ZagSmalltalkTest >> scan: aString [

	^ ZagSmalltalk new statementForTest: aString
]

{ #category : 'tests' }
ZagSmalltalkTest >> testCascade [

	self methodFrom: 'cascade

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
		abs'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #cascade.
	self assert: method scope asArray equals: #( foo ).
	self assert: method body size equals: 17
]

{ #category : 'tests' }
ZagSmalltalkTest >> testGives43 [

	self methodFrom: 'gives43
	| forty2 |
	forty2 := self unaries * 2.
	^ self plus1: (forty2 - 4)'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #gives43.
	self assert: method scope asArray equals: #( forty2 ).
	self assert: method body size equals: 2
]

{ #category : 'tests' }
ZagSmalltalkTest >> testLiterals [

	self assert: (self scan: '"foo" nil') baseValue equals: nil.
	self assertEquivalent: 42.
	self assertEquivalent: #hello.
	self assertEquivalent: #_hello.
	self assertEquivalent: #nil.
	self expectError: '#42'.
	self expectError: '# hello'.
	self assertEquivalent: 'abc'.
	self assertEquivalent: nil.
	self assertEquivalent: #( 1 2 3 ).
	self assertEquivalent: #( 1 #( 2 abc ) #( nitrue false nil ) )
]

{ #category : 'tests' }
ZagSmalltalkTest >> testOr [

	self methodFrom: '| n1
	| temp |
	temp := self | n1.
	^ temp'.
	self assert: method arguments equals: #( n1 ).
	self assert: method selector equals: #|.
	self assert: method scope asArray equals: #( n1 temp ).
	self assert: method body size equals: 2
]

{ #category : 'tests' }
ZagSmalltalkTest >> testOrOr [

	self methodFrom: '|| n1
	| temp temp2 |
	temp := temp2 :=self || n1.
	^ temp'.
	self assert: method arguments equals: #( n1 ).
	self assert: method selector equals: #'||'
]

{ #category : 'tests' }
ZagSmalltalkTest >> testPlus [

	self methodFrom: '+ n1
	| temp |
	temp := self + n1.
	^ temp'.
	self assert: method arguments equals: #( n1 ).
	self assert: method selector equals: #+
]

{ #category : 'tests' }
ZagSmalltalkTest >> testPlus1 [

	self methodFrom: 'plus1: _Number ^ _Number + 1'.
	self assert: method arguments equals: #( _Number ).
	self assert: method selector equals: #plus1:
]

{ #category : 'tests' }
ZagSmalltalkTest >> testPlusAndAnd [

	self methodFrom: 'plus: n1 and: n2 and: n3

	^ 5 + (n1 + (n2 + n3)) + 4'.
	self assert: method arguments equals: #( n1 n2 n3 ).
	self assert: method selector equals: #plus:and:and:
]

{ #category : 'tests' }
ZagSmalltalkTest >> testPlusStar [

	self methodFrom: '+ n1
	^ n1'.
	self assert: method arguments size equals: 1.
	self assert: method selector equals: #+.
	self assert: method printString equals: '* n1

	^ n1'
]

{ #category : 'tests' }
ZagSmalltalkTest >> testPrint [

	self assert: 42 asZagLiteral printString equals: '42'
]

{ #category : 'tests' }
ZagSmalltalkTest >> testSimple1 [

	self methodFrom: 'simple1
	..
	^ 42..'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #simple1.
	self assert: method printString equals: 'simple1

	^ 42'
]

{ #category : 'tests' }
ZagSmalltalkTest >> testSimple2 [

	self methodFrom: 'simple2: foo
	^ 42'.
	self assert: method arguments size equals: 1.
	self assert: method arguments first id equals: #foo.
	self assert: method selector equals: #simple2:.
	self assert: method printString equals: 'simple2: foo

	^ 42'
]

{ #category : 'tests' }
ZagSmalltalkTest >> testUnaries [

	self methodFrom: 'unaries ^ 23 negated "test comment" abs'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #unaries
]
