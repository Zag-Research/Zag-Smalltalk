"
I am the generic text outputter for LLVM
"
Class {
	#name : 'ASLLOutput',
	#superclass : 'ASTextOutput',
	#category : 'Zag-Smalltalk-LLVM',
	#package : 'Zag-Smalltalk-LLVM'
}

{ #category : 'accessing' }
ASLLOutput >> label: anObject [ 

	anObject ifNotNil: [ mainStream << anObject << $:;lf ]
]
