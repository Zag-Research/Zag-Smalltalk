Class {
	#name : 'ASStackTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASStackTest >> testLookupContext [

	| stack result |
	stack := ASStack new.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	stack := ASStackContext fromStack: stack.
	stack := ASStackTemporary fromStack: stack.
	stack top: (ASCClassValue new init: SmallInteger class).

	result := stack
		          offset: 0
		          to: #p1
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #A1.

	result := stack
		          offset: 0
		          to: #p2
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #A0
]

{ #category : 'tests' }
ASStackTest >> testNames [

	| stack |
	stack := ASStack new.
	stack := stack pushSelf.
	stack := stack initParameterNamed: #p1.
	stack := stack initParameterNamed: #p2.
	stack := stack initLocalNamed: #l1.
	stack at: #p1 put: 17.
	stack at: #l1 put: 42.
	stack := stack push: 99.
	self assert: stack drop drop drop value equals: 17.
	self assert: (stack at: #p1) offset equals: -2.
	self deny: (stack at: #p1) isInContext.
	stack := stack makeContextRequired: [  ].
	self assert: (stack at: #p1) offset equals: 2.
	self assert: (stack at: #p1) isInContext.
	stack := stack push: 12.
	stack := stack push: 27.
	self assert: (stack at: 2) value equals: 99.
	stack := stack initLocalNamed: #b1.
	stack := stack push: 98.
	self assert: (stack at: #b1) offset equals: -4.
	self deny: (stack at: #b1) isInContext.
	self assert: (stack at: #l1) value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSelf [

	| stack2 stack3 stack4 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new push: 42) drop isEmpty.
	self assert: (ASStack new push: 42) value equals: 42.
	stack2 := (ASStack new push: 42) push: 17.
	self assert: stack2 value equals: 17.
	self assert: stack2 drop value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 push: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42.
	
	stack4 := stack3 push.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSimple [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new push: 42) drop isEmpty.
	self assert: (ASStack new push: 42) value equals: 42.
	stack2 := (ASStack new push: 42) push: 17.
	self assert: stack2 value equals: 17.
	self assert: stack2 drop value equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 push: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: (stack3 drop: 2) value equals: 42
]
