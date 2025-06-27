"
A ZagOutputLLVMTest is a test class for testing the behavior of ZagOutputLLVM
"
Class {
	#name : 'ZagJITTest',
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
ZagJITTest class >> fibWithPIC [

	^ #( pushContext 16r0 pushLocal 0 pushLiteral 2 sendPIC2 #'<=' nil
	     nil nil nil pushThisContext makeImmediateClosure
	     16r119 sendPIC2 ifTrue: nil nil nil nil drop pushLocal 0 pushLiteral
	     1 sendPIC2 #- nil nil nil nil call #( method ) pushLocal
	     0 pushLiteral 2 sendPIC2 #- nil nil nil nil call #( method )
	     sendPIC2 #+ nil nil nil nil returnTop )
]

{ #category : 'tests' }
ZagJITTest class >> fibWithoutPIC [

	^ #( pushContext #( method ) pushLocal 0 pushLiteral 2 send #'<='
	     pushThisContext makeImmediateClosure 16r119 send ifTrue:
	     drop pushLocal 0 pushLiteral 1 send #- call #( method )
	     pushLocal 0 pushLiteral 2 send #- call #( method ) send #+ returnTop )
]

{ #category : 'tests' }
ZagJITTest class >> return42 [

	^ #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ZagJITTest class >> returnAdd42 [

	^ #( pushContext #( method ) pushLiteral 25 pushLiteral 7 send #+ returnTop )
]

{ #category : 'tests' }
ZagJITTest class >> returnAdd42Tail [

	^ #( pushLiteral 25 pushLiteral 7 tailSend #+ )
]

{ #category : 'running' }
ZagJITTest >> setUp [

	super setUp.
	generator := ZagJITPrimitiveGenerator new.
	driver := ZagJITDriver new
		          methodAddress: 16r10000; "address of the compiledMethod"
		          dispatcher: (ZagJITDispatcher new generator: generator).
]

{ #category : 'tests' }
ZagJITTest >> testAdd [

	driver primitiveSmallIntegerPlus.
	generator contents inspect
]

{ #category : 'tests' }
ZagJITTest >> testFibFromArray [

	driver genFrom: self class fibWithoutPIC
]

{ #category : 'tests' }
ZagJITTest >> testFromArray [

   "JIT compile the #return42 compiledMethod"
	driver genFrom: self class return42.
	generator inspect
]
