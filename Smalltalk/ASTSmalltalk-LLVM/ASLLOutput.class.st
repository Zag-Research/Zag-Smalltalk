"
I am the generic text outputter for LLVM
"
Class {
	#name : 'ASLLOutput',
	#superclass : 'ASTextOutput',
	#category : 'ASTSmalltalk-LLVM',
	#package : 'ASTSmalltalk-LLVM'
}

{ #category : 'accessing' }
ASLLOutput >> label: anObject [ 

	anObject ifNotNil: [ mainStream << anObject << $:;lf ]
]
