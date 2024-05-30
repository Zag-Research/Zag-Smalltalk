Class {
	#name : 'ASStackTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASStackTest >> testLookupBlocks [

	| stack result cg output |
	stack := ASEmptyStack new.
	stack := ASStackBlockClosure fromStack: stack.

	output := ASZigThreadedOutput new.
	output context: ASZigFileContext new.
	output codeGenerator.
	cg := ASCodeGenerator new.
	cg output: output.
	cg selector: #test.
	cg locals: { #mb1 } parameters: { #aSample }.

	stack nonlocalNameLookupObject: cg.
	stack := ASStackNamed fromStack: stack.
	stack name: #p1.
	stack := ASStackNamed fromStack: stack.
	stack name: #p2.
	stack := ASStackContext fromStack: stack.
	stack := ASStackElement fromStack: stack.
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
	self assert: result equals: #A0.

	result := stack
		          offset: 0
		          to: #mb1
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :o1 :o2 :o3 |
		          #C , o1 asString , o2 asString , o3 asString ].
	self assert: result equals: #C210
]

{ #category : 'tests' }
ASStackTest >> testLookupContext [

	| stack result |
	stack := ASEmptyStack new.
	stack := ASStackNamed fromStack: stack.
	stack name: #p1.
	stack := ASStackNamed fromStack: stack.
	stack name: #p2.
	stack := ASStackContext fromStack: stack.
	stack := ASStackElement fromStack: stack.
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
ASStackTest >> testLookupContextPlacehoder [

	| stack result |
	stack := ASEmptyStack new.
	stack := ASStackNamed fromStack: stack.
	stack name: #p1.
	stack := ASStackNamed fromStack: stack.
	stack name: #p2.
	stack := ASStackContextPlaceholder fromStack: stack.
	stack := ASStackElement fromStack: stack.
	stack top: (ASCClassValue new init: SmallInteger class).

	result := stack
		          offset: 0
		          to: #p1
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B2.

	result := stack
		          offset: 0
		          to: #p2
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B1
]

{ #category : 'tests' }
ASStackTest >> testLookupJob [

	| stack result |
	stack := ASEmptyStack new.
	stack := ASStackNamed fromStack: stack.
	stack name: #p1.
	stack := ASStackNamed fromStack: stack.
	stack name: #p2.
	stack := ASStackNamed fromStack: stack.
	stack name: #p3.

	result := stack
		          offset: 0
		          to: #p1
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B2.

	result := stack
		          offset: 0
		          to: #p2
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B1.

	result := stack
		          offset: 0
		          to: #p3
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B0
]

{ #category : 'tests' }
ASStackTest >> testLookupParameters [

	| stack result |
	stack := ASEmptyStack new.
	stack := ASStackNamed fromStack: stack.
	stack name: #p1.
	stack := ASStackNamed fromStack: stack.
	stack name: #p2.
	stack := ASStackNamed fromStack: stack.
	stack name: #p3.

	result := stack
		          offset: 0
		          to: #p1
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B2.

	result := stack
		          offset: 0
		          to: #p2
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B1.

	result := stack
		          offset: 0
		          to: #p3
		          context: [ :offest | #A , offest asString ]
		          stack: [ :offest | #B , offest asString ]
		          other: [ :offest | #C , offest asString ].
	self assert: result equals: #B0
]

{ #category : 'tests' }
ASStackTest >> testNames [

	| stack |
	stack := ASStack new.
	stack := stack pushParameterNamed: #self.
	stack := stack pushParameterNamed: #p1.
	stack := stack pushParameterNamed: #p2.
	stack := stack pushLocalNamed: #l1.
	stack at: #p1 put: 17.
	stack at: #l1 put: 42.
	stack := stack push: 99.
	self assert: stack drop drop drop top equals: 17.
	self assert: (stack offset: 0 to: #p1) equals: 3.
	stack := stack pushPlaceholderContext.
	stack := stack push: 12.
	stack := stack push: 27.
	stack := stack pushLocalNamed: #b1.
	stack := stack push: 98.
	self assert: (stack offset: 0 to: #b1) equals: 1.
	self assert: (stack at: #l1) equals: 42
]

{ #category : 'tests' }
ASStackTest >> testSimple [

	| stack2 stack3 |
	self assert: ASEmptyStack new isEmpty.
	self assert: (ASEmptyStack new push: 42) drop isEmpty.
	self assert: (ASEmptyStack new push: 42) top equals: 42.
	stack2 := (ASEmptyStack new push: 42) push: 17.
	self assert: stack2 top equals: 17.
	self assert: stack2 drop top equals: 42.
	self assert: stack2 drop drop isEmpty.
	stack3 := stack2 push: 99.
	self assert: stack2 drop drop isEmpty.
	self deny: stack3 drop drop isEmpty.
	self assert: stack2 depth equals: 2.
	self assert: (stack3 drop: 2) top equals: 42
]
