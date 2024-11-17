Class {
	#name : 'ASStackTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASStackTest >> testLookupContext [

	| stack context |
	stack := ASStack new.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	self assert: stack offset equals: 2.
	stack := context := ASStackContext fromStack: stack.
	self assert: stack offset equals: 2.
	stack := stack pushForTests: 42.
	context required;growBy: 2.
	self assert: context size equals: 5.

	self deny: (stack at: 0) isInContext.
	self assert: (stack at: #p1) isInContext.
	self assert: (stack at: #p1) offset equals: 1.
	self assert: (stack at: #p2) isInContext.
	self assert: (stack at: #p2) offset equals: 0
]

{ #category : 'tests' }
ASStackTest >> testNames [

	| stack context |
	stack := ASStack new.
	stack := stack initSelf.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	stack := stack initLocalNamed: #l1.
	self assert: stack offset equals: 4.
	stack := context := ASStackContext fromStack: stack.
	self assert: stack offset equals: 4.
	self assert: context size equals: 0.
	stack at: #p1 put: 17.
	stack at: #l1 put: 42.
	stack := stack pushForTests: 99.
	self assert: (stack at: 0) stackValue value equals: 99.
	self assert: (stack at: #p1) offset equals: 2.
	self deny: (stack at: #p1) isInContext.
	context required.
	self assert: stack offset equals: 5.
	self assert: context size equals: 4.
	self assert: stack offset equals: 5.
	self assert: (stack at: #p1) offset equals: 2.
	self assert: (stack at: #p1) isInContext.
	stack := stack pushForTests: 12.
	stack := stack pushForTests: 27.
	self assert: (stack at: 2) stackValue value equals: 99.
	stack := stack initLocalNamed: #b1.
	stack := stack pushForTests: 98.
	self assert: (stack at: #b1) offset equals: 8.
	self deny: (stack at: #b1) isInContext.
	self assert: (stack at: #l1) stackValue value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSelf [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new pushForTests: 42) drop isEmpty.
	self assert: (ASStack new pushForTests: 42) stackValue value equals: 42.
	stack2 := (ASStack new pushForTests: 42) pushForTests: 17.
	self assert: stack2 stackValue value equals: 17.
	self assert: stack2 drop stackValue value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 pushForTests: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) stackValue value equals: 42.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) stackValue value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSimple [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new pushForTests: 42) drop isEmpty.
	self assert: (ASStack new pushForTests: 42) stackValue value equals: 42.
	stack2 := (ASStack new pushForTests: 42) pushForTests: 17.
	self assert: stack2 stackValue value equals: 17.
	self assert: stack2 drop stackValue value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 pushForTests: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) stackValue value equals: 42
]
