Class {
	#name : 'ASLLOutput',
	#superclass : 'ASTextOutput',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'accessing' }
ASLLOutput >> label: anObject [ 

	anObject ifNotNil: [ mainStream << anObject << $:;lf ]
]
