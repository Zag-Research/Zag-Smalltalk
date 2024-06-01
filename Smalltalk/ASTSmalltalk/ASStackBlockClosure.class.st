"
I represent a block with a block closure and help find indirect variables. 
"
Class {
	#name : 'ASStackBlockClosure',
	#superclass : 'ASStack',
	#instVars : [
		'nonlocalNameLookupObject',
		'blockClosure'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASStackBlockClosure >> depth [
	^0
]

{ #category : 'accessing' }
ASStackBlockClosure >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : 'instance creation' }
ASStackBlockClosure >> initialize [

	super initialize.
	blockClosure := OrderedCollection new
]

{ #category : 'accessing' }
ASStackBlockClosure >> lookUp: methodName [
	blockClosure indexOf: methodName ifAbsent: [ blockClosure add: methodName].
	^ blockClosure indexOf: methodName 
]

{ #category : 'API' }
ASStackBlockClosure >> nonlocalNameLookupObject: anASCodeGenerator [

		nonlocalNameLookupObject := anASCodeGenerator
]

{ #category : 'API' }
ASStackBlockClosure >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [

	| offsetToTheObject offsetToTheMethod |
	"Offset to the object is a value that will be retrived by the nonlocaNameLookupOjbect.
	When the offset is found the name of the context will be passed to the methodBlock given
	to get the offset to corresponding named context "
	offsetToTheMethod := nil.
	offsetToTheObject := nonlocalNameLookupObject
		                     offsetTo: anObject
		                     methodBlock: [ :name |
			                     offsetToTheMethod ifNil: [
				                     offsetToTheMethod := self lookUp: name ] ].
	^ blockOther
		  value: anInteger
		  value: offsetToTheMethod
		  value: offsetToTheObject
]

{ #category : 'as yet unclassified' }
ASStackBlockClosure >> offsetToSelf: anInteger [ 
self halt.
	^ rest offsetToSelf: anInteger + 1
]

{ #category : 'accessing' }
ASStackBlockClosure >> requiredContextDo: aBlock [ 
	^ rest requiredContextDo: aBlock
]
