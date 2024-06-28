"
Clean up stack and return self
"
Class {
	#name : 'ASCReturnSelf',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCReturnSelf >> next [

	^ nil
]

{ #category : 'code generation' }
ASCReturnSelf >> outputTo: output [

	^ output visitReturnSelf
]

{ #category : 'stack' }
ASCReturnSelf >> resultStack: anASStack [

	^ anASStack at: #self
]

{ #category : 'instance creation' }
ASCReturnSelf >> with: ignored [


]
