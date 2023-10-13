Class {
	#name : #ASStackTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASStackTest >> testNames [

	| stack |
	stack := ASStack new push: 42.
	stack := stack stackNames: #( l1 p2 p1 self ).
	stack at: #p1 put: 17.
	stack := stack push: 99.
	self assert: stack drop drop drop top equals: 17.
	self assert: (stack offsetTo: #p1) equals: 3.
	stack := stack pushContext.
	stack := stack push: 12.
	stack := stack push: 27.
	stack := stack stackNames: #( b1 ).
	stack := stack push: 98.
	self assert: (stack offsetTo: #b1) equals: 1.
	self assert: (stack at: #l1) equals: 42
]

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
