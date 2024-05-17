"
I am a named stack element (i.e. a local).
"
Class {
	#name : 'ASStackNamed',
	#superclass : 'ASStackElement',
	#instVars : [
		'name'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASStackNamed >> at: aSymbol put: anObject [

	aSymbol = name ifFalse: [ ^
	rest at: aSymbol put: anObject].
	^ self top: anObject
]

{ #category : 'accessing' }
ASStackNamed >> find: aSymbol [

	aSymbol = name ifTrue: [ ^ self ].
	^ rest find: aSymbol
]

{ #category : 'accessing' }
ASStackNamed >> llvmName [

	^ llvmName ifNil: [
		  llvmName := super llvmName, name ]
]

{ #category : 'accessing' }
ASStackNamed >> name: aSymbol [
	name := aSymbol
]

{ #category : 'API' }
ASStackNamed >> offset: anInteger to: aName [

	aName = name ifTrue: [ ^ anInteger ].
	^ rest
		  offset: anInteger + 1
		  to: aName
]

{ #category : 'API' }
ASStackNamed >> offset: anInteger to: aName local: blockLocal stack: blockStack other: blockOther [

	aName = name ifTrue: [ ^ blockStack cull: anInteger cull: self ].
	^ rest
		  offset: anInteger + 1
		  to: aName
		  local: blockLocal
		  stack: blockStack
		  other: blockOther
]
