Class {
	#name : 'ASStackTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASStackTest >> testLookupContext [

	| stack |
	stack := ASStack new.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	stack := stack makeContextRequired: [  ].
	stack := stack pushForTests: (ASCClassValue new init: SmallInteger class).

	self deny: (stack at: 0) isInContext.
	self assert: (stack at: #p1) isInContext.
	self assert: (stack at: #p1) offset equals: 1.
	self assert: (stack at: #p2) isInContext.
	self assert: (stack at: #p2) offset equals: 0
]

{ #category : 'tests' }
ASStackTest >> testNames [

	| stack |
	stack := ASStack new.
	stack := stack initSelf.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	stack := stack initLocalNamed: #l1.
	stack at: #p1 put: 17.
	stack at: #l1 put: 42.
	stack := stack pushForTests: 99.
	self assert: (stack at: 0) value equals: 99.
	self assert: (stack at: #p1) offset equals: -2.
	self deny: (stack at: #p1) isInContext.
	stack := stack makeContextRequired: [  ].
	self assert: stack offset equals: -1.
	self assert: (stack at: #p1) offset equals: 2.
	self assert: (stack at: #p1) isInContext.
	stack := stack pushForTests: 12.
	stack := stack pushForTests: 27.
	self assert: (stack at: 2) value equals: 99.
	stack := stack initLocalNamed: #b1.
	stack := stack pushForTests: 98.
	self assert: (stack at: #b1) offset equals: -4.
	self deny: (stack at: #b1) isInContext.
	self assert: (stack at: #l1) value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSelf [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new pushForTests: 42) drop isEmpty.
	self assert: (ASStack new pushForTests: 42) value equals: 42.
	stack2 := (ASStack new pushForTests: 42) pushForTests: 17.
	self assert: stack2 value equals: 17.
	self assert: stack2 drop value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 pushForTests: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSimple [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new pushForTests: 42) drop isEmpty.
	self assert: (ASStack new pushForTests: 42) value equals: 42.
	stack2 := (ASStack new pushForTests: 42) pushForTests: 17.
	self assert: stack2 value equals: 17.
	self assert: stack2 drop value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 pushForTests: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42.
	self assert: (stack3 dropUnder: 1) value equals: 99.
	self assert: (stack3 dropUnder: 1) drop value equals: 42
]
