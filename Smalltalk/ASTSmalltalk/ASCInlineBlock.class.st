"
for inlining
"
Class {
	#name : 'ASCInlineBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'method'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCInlineBlock class >> for: anASCBlock [ 
	^ super new method: anASCBlock namedMethod
]

{ #category : 'instance creation' }
ASCInlineBlock class >> new [

	self shouldNotImplement
]

{ #category : 'initialization' }
ASCInlineBlock >> label [

	^ label ifNil: [ label := method nextSequence asString ]
]

{ #category : 'accessing' }
ASCInlineBlock >> method: anASCMethodBlock [ 
	method := anASCMethodBlock
]

{ #category : 'as yet unclassified' }
ASCInlineBlock >> namedMethod [
	^ method namedMethod
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: self label
]
