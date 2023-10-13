"
I rempresent a stack context.

I may,  or may not be reified in a particular method
"
Class {
	#name : #ASStackContext,
	#superclass : #ASStack,
	#instVars : [
		'rest',
		'size'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASStackContext class >> from: anASStack [
	^ self basicNew from: anASStack 
]

{ #category : #accessing }
ASStackContext >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : #API }
ASStackContext >> findContext [ 
^ self
]

{ #category : #'instance creation' }
ASStackContext >> from: anASStack [

	rest := anASStack
]

{ #category : #initialization }
ASStackContext >> initialize [ 

super initialize.
size := 0
]

{ #category : #API }
ASStackContext >> offset: anInteger to: aSymbol [

	^ size = 0
		  ifTrue: [ rest offset: anInteger to: aSymbol ]
		  ifFalse: [ rest find: aSymbol ]
]

{ #category : #API }
ASStackContext >> pushContext [ 
self shouldNotImplement 
]
