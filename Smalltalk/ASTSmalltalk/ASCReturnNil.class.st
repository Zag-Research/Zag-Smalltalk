"
Clean up stack and return nil
"
Class {
	#name : 'ASCReturnNil',
	#superclass : 'ASCOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCReturnNil >> next [

	^ nil
]

{ #category : 'code generation' }
ASCReturnNil >> outputTo: output [

	^ output visitReturnNil
]

{ #category : 'instance creation' }
ASCReturnNil >> with: ignored [


]
