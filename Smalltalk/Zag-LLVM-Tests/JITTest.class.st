"
A ZagOutputLLVMTest is a test class for testing the behavior of ZagOutputLLVM
"
Class {
	#name : 'JITTest',
	#superclass : 'TestCase',
	#instVars : [
		'driver',
		'generator'
	],
	#category : 'Zag-LLVM-Tests-Tests',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
JITTest class >> fibWithPIC [

	^ #( pushContext 16r0 pushLocal 0 pushLiteral 2 sendPIC2 #'<=' nil
	     nil nil nil pushThisContext makeImmediateClosure
	     16r119 sendPIC2 ifTrue: nil nil nil nil drop pushLocal 0 pushLiteral
	     1 sendPIC2 #- nil nil nil nil call #( method ) pushLocal
	     0 pushLiteral 2 sendPIC2 #- nil nil nil nil call #( method )
	     sendPIC2 #+ nil nil nil nil returnTop )
]

{ #category : 'tests' }
JITTest class >> fibWithoutPIC [

	^ #( pushContext #( method ) pushLocal 0 pushLiteral 2 send #'<='
	     pushThisContext makeImmediateClosure 16r119 send ifTrue:
	     drop pushLocal 0 pushLiteral 1 send #- call #( method )
	     pushLocal 0 pushLiteral 2 send #- call #( method ) send #+ returnTop )
]

{ #category : 'tests' }
JITTest class >> return42 [

	^ #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
JITTest class >> returnAdd42 [

	^ #( pushContext #( method ) pushLiteral 25 pushLiteral 7 send #+ returnTop )
]

{ #category : 'tests' }
JITTest class >> returnAdd42Tail [

	^ #( pushLiteral 25 pushLiteral 7 tailSend #+ )
]

{ #category : 'running' }
JITTest >> setUp [

	super setUp.
	generator := JITPrimitiveGenerator new.
	driver := JITDriver new
		          methodAddress: 16r10000; "address of the compiledMethod"
		          dispatcher: (JITDispatcher new generator: generator).
]

{ #category : 'tests' }
JITTest >> testAdd [

	driver primitiveSmallIntegerPlus.
	generator contents inspect
]

{ #category : 'tests' }
JITTest >> testFibFromArray [

	driver genFrom: self class fibWithoutPIC
]

{ #category : 'tests' }
JITTest >> testFromArray [

   "JIT compile the #return42 compiledMethod"
	driver genFrom: self class return42.
	generator inspect
]
