"
I rempresent a stack context.

I may,  or may not be reified in a particular method
"
Class {
	#name : 'ASStackContextPlaceholder',
	#superclass : 'ASStack',
	#instVars : [
		'rest'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASStackContextPlaceholder class >> from: anASStack [
	^ self basicNew from: anASStack 
]

{ #category : 'accessing' }
ASStackContextPlaceholder >> depth [
	^ rest depth
]

{ #category : 'accessing' }
ASStackContextPlaceholder >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : 'instance creation' }
ASStackContextPlaceholder >> from: anASStack [

	rest := anASStack
]

{ #category : 'API' }
ASStackContextPlaceholder >> makeContextRequired [
	^ ASStackContext from: rest
]

{ #category : 'API' }
ASStackContextPlaceholder >> offset: anInteger to: anObject local: blockLocal stack: blockStack other: blockOther [

	^ rest
		  offset: anInteger
		  to: anObject
		  local: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'API' }
ASStackContextPlaceholder >> pushPlaceholderContext [ 
self shouldNotImplement 
]

{ #category : 'enumerating' }
ASStackContextPlaceholder >> requiredContextDo: aBlock [

	^ self
]
