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

	^ #( #( label ) pushContext 16r0 pushLocal 0 pushLiteral 2 send #'<='
	     nil nil nil nil #( label ) #pushThisContext #makeImmediateClosure
	     16r119 send ifTrue: nil nil nil nil #( label ) drop pushLocal
	     0 pushLiteral 1 send #- nil nil nil nil #( label ) call #( method )
	     pushLocal 0 pushLiteral 2 send #- nil nil nil nil #( label )
	     call #( method ) send #+ nil nil nil nil #( label ) returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> fibWithoutPIC [

	^ #( #( label ) pushContext #( method ) pushLocal 0 pushLiteral
	     2 send #'<=' #( label ) #pushThisContext #makeImmediateClosure
	     16r119 send ifTrue: #( label ) drop pushLocal 0 pushLiteral
	     1 send #- #( label ) call #( method ) #( label ) pushLocal
	     0 pushLiteral 2 send #- #( label ) call #( method ) #( label )
	     send #+ #( label ) returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> return42 [

	^ #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> returnAdd42 [

	^ #( pushContext #( method ) pushLiteral 25 pushLiteral 7 send #+ label
	     returnTop )
]

{ #category : 'running' }
ZagOutputLLVMTest >> setUp [

	super setUp.
	textOut := ZagOutputLLVMTextual new.
	output := ZagOutputLLVMForTest new
		          address: 16r10000;
		          codeGen: (ZagOutputLLVM new generator: textOut)
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
