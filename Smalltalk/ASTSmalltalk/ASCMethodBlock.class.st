"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : #ASCMethodBlock,
	#superclass : #ASCBasicBlock,
	#instVars : [
		'primitive'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #initialization }
ASCMethodBlock >> initialize [ 
super initialize.
	self stack: ASStack new.

]

{ #category : #wriiting }
ASCMethodBlock >> outputTo: output [

	output outputMethodStart: label.
	primitive
		ifNil: [ output verifySelector ]
		ifNotNil: [ output primitive: primitive ].
	output outputMethodLabel: label.
	self outputRestTo: output
]

{ #category : #accessing }
ASCMethodBlock >> primitive: anInteger [ 
	primitive := anInteger 
]
