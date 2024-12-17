Class {
	#name : 'ASImageLLVMOutput',
	#superclass : 'Object',
	#instVars : [
		'coder'
	],
	#category : 'Zag-LLVM',
	#package : 'Zag-LLVM'
}

{ #category : 'API' }
ASImageLLVMOutput >> visitPushLiteral: literal [
	" %v27 = u64 42 "
	"test"
	self push: (coder assignLiteral: literal)
]

{ #category : 'API' }
ASImageLLVMOutput >> visitReturnTop [
	" ret %v27 "

	coder return: self pop
]
