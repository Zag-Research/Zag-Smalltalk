Class {
	#name : #ASStackTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASStackTest >> testSimple [

	| stack2 stack3 |
	self assert: ASStack new isEmpty.
	self assert: (ASStack new push: 42) drop isEmpty.
	self assert: (ASStack new push: 42) top equals: 42.
	stack2 := (ASStack new push: 42) push: 17.
	self assert: stack2 top equals: 17.
	self assert: stack2 drop top equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 push: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: stack2 depth equals: 2.
	self assert: (stack3 drop: 2) top equals: 42
]
