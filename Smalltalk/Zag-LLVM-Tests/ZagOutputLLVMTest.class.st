"
A ZagOutputLLVMTest is a test class for testing the behavior of ZagOutputLLVM
"
Class {
	#name : 'ZagOutputLLVMTest',
	#superclass : 'TestCase',
	#instVars : [
		'output',
		'textOut'
	],
	#category : 'Zag-LLVM-Tests-Tests',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ZagOutputLLVMTest class >> fibWithPIC [

	^ #( pushContext 16r0 pushLocal 0 pushLiteral 2 sendPIC2 #'<=' nil
	     nil nil nil pushThisContext makeImmediateClosure
	     16r119 sendPIC2 ifTrue: nil nil nil nil drop pushLocal 0 pushLiteral
	     1 sendPIC2 #- nil nil nil nil call #( method ) pushLocal
	     0 pushLiteral 2 sendPIC2 #- nil nil nil nil call #( method )
	     sendPIC2 #+ nil nil nil nil returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> fibWithoutPIC [

	^ #( pushContext #( method ) pushLocal 0 pushLiteral 2 send #'<='
	     pushThisContext makeImmediateClosure 16r119 send ifTrue:
	     drop pushLocal 0 pushLiteral 1 send #- call #( method )
	     pushLocal 0 pushLiteral 2 send #- call #( method ) send #+ returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> return42 [

	^ #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> returnAdd42 [

	^ #( pushContext #( method ) pushLiteral 25 pushLiteral 7 send #+ returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> returnAdd42Tail [

	^ #( pushLiteral 25 pushLiteral 7 tailSend #+ )
]

{ #category : 'running' }
ZagOutputLLVMTest >> setUp [

	super setUp.
	textOut := JITPrimitiveGenerator new.
	output := ZagOutputLLVMFromTestArray new
		          methodAddress: 16r10000;
		          codeGen: (JITDispatcher new generator: textOut)
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testAdd [

	output primitiveSmallIntegerPlus.
	textOut contents inspect
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testFibFromArray [

	output genFrom: self class fibWithoutPIC
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testFromArray [

	output halt; genFrom: self class return42.
	textOut contents inspect
]
