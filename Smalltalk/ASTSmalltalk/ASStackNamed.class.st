"
I am a named stack element (i.e. a local).
"
Class {
	#name : #ASStackNamed,
	#superclass : #ASStackElement,
	#instVars : [
		'name'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASStackNamed >> at: aSymbol put: anObject [

	aSymbol = name ifFalse: [ ^
	rest at: aSymbol put: anObject].
	^ self top: anObject
]

{ #category : #accessing }
ASStackNamed >> find: aSymbol [

	aSymbol = name ifTrue: [ ^ self ].
	^ rest find: aSymbol
]

{ #category : #accessing }
ASStackNamed >> name: aSymbol [

	name := aSymbol
]

{ #category : #API }
ASStackNamed >> offset: anInteger to: anObject local: blockLocal stack: blockStack [

	anObject = name ifTrue: [ ^ blockStack value: anInteger ].
	^ rest
		  offset: anInteger + 1
		  to: anObject
		  local: blockLocal
		  stack: blockStack
]
