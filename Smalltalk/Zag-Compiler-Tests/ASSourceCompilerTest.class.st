Class {
	#name : 'ASSourceCompilerTest',
	#superclass : 'TestCase',
	#instVars : [
		'method'
	],
	#category : 'Zag-Compiler-Tests-Tests',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Tests'
}

{ #category : 'asserting' }
ASSourceCompilerTest >> assertEquivalent: anObject [ 
	| result |
	result := ASSourceCompiler new statementForTest: anObject printString.
	self assert: result baseValue equals: anObject.

]

{ #category : 'asserting' }
ASSourceCompilerTest >> expectError: aString [

	self should: [ ASSourceCompiler new statementForTest: aString ] raise: Error
]

{ #category : 'tests' }
ASSourceCompilerTest >> methodFrom: aString [

	method := ASSourceCompiler new
		  compileMethod: aString
		  inClass: (ASClassNode new from: self class)
]

{ #category : 'asserting' }
ASSourceCompilerTest >> scan: aString [

	^ ASSourceCompiler new statementForTest: aString
]

{ #category : 'tests' }
ASSourceCompilerTest >> testCascade [

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
ASSourceCompilerTest >> testGives43 [

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
ASSourceCompilerTest >> testLiterals [

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
ASSourceCompilerTest >> testOr [

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
ASSourceCompilerTest >> testOrOr [

	self methodFrom: '|| n1
	| temp temp2 |
	temp := temp2 :=self || n1.
	^ temp'.
	self assert: method arguments equals: #( n1 ).
	self assert: method selector equals: #'||'
]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlus [

	self methodFrom: '+ n1
	| temp |
	temp := self + n1.
	^ temp'.
	self assert: method arguments equals: #( n1 ).
	self assert: method selector equals: #+
]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlus1 [

	self methodFrom: 'plus1: _Number ^ _Number + 1'.
	self assert: method arguments equals: #( _Number ).
	self assert: method selector equals: #plus1:
]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlusAndAnd [

	self methodFrom: 'plus: n1 and: n2 and: n3

	^ 5 + (n1 + (n2 + n3)) + 4'.
	self assert: method arguments equals: #( n1 n2 n3 ).
	self assert: method selector equals: #plus:and:and:
]

{ #category : 'tests' }
ASSourceCompilerTest >> testSimple1 [

	self methodFrom: 'simple1
	..
	^ 42..'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #simple1
]

{ #category : 'tests' }
ASSourceCompilerTest >> testUnaries [

	self methodFrom: 'unaries ^ 23 negated abs'.
	self assert: method arguments equals: #(  ).
	self assert: method selector equals: #unaries
]
