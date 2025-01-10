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

	^ #( #( label ) pushContext 16r0 pushLocal 0 pushLiteral 2 send1
	     #'<=' nil nil nil nil #( label ) #pushThisContext
	     #makeImmediateClosure 16r119 send1 ifTrue: nil nil nil nil #( label )
	     drop pushLocal 0 pushLiteral 1 send1 #- nil nil nil nil #( label )
	     call #( method ) pushLocal 0 pushLiteral 2 send1 #- nil nil nil
	     nil #( label ) call #( method ) send1 #+ nil nil nil nil #( label )
	     returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> fibWithoutPIC [

	^ #( #( label ) pushContext 16r0 pushLocal 0 pushLiteral 2 send1
	     #'<=' #( label ) #pushThisContext #makeImmediateClosure
	     16r119 send1 ifTrue: #( label ) drop pushLocal 0 pushLiteral
	     1 send1 #- #( label ) call #( method ) pushLocal 0 pushLiteral
	     2 send1 #- #( label ) call #( method ) send1 #+ #( label )
	     returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest class >> return42 [

	^ #( pushContext #( method ) pushLiteral 25 pushLiteral 7 send1 #+ #( label )
	     returnTop )
]

{ #category : 'tests' }
ZagOutputLLVMTest >> fibWithPIC [
	^ self class fibWithPIC
]

{ #category : 'tests' }
ZagOutputLLVMTest >> fibWithoutPIC [
	^ self class fibWithoutPIC
]

{ #category : 'tests' }
ZagOutputLLVMTest >> return42 [
	^ self class return42
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

	output genFrom: self fibWithoutPIC
]

{ #category : 'tests' }
ZagOutputLLVMTest >> testFromArray [

	output genFrom: self return42
]
