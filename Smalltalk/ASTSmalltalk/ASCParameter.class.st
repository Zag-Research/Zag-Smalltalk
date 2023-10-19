"
I am a parameter for a method.
ASCSelf is a subclass
"
Class {
	#name : #ASCParameter,
	#superclass : #ASCValue,
	#instVars : [
		'offset',
		'name',
		'stack'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASCParameter class >> inBasicBlock: anASCBasicBlock [ 
	^ self new inBasicBlock: anASCBasicBlock 
]

{ #category : #'instance initialization' }
ASCParameter >> inBasicBlock: anASCBasicBlock [ 
	stack := anASCBasicBlock stack
]

{ #category : #accessing }
ASCParameter >> name [ 
^ name
]

{ #category : #accessing }
ASCParameter >> name: aSymbol [

	name := aSymbol 
]

{ #category : #accessing }
ASCParameter >> offset [

	^ stack offsetTo: self name
]
