"
I contain pragmas for a method - the only one we currently care about is <primitive: 1>
"
Class {
	#name : 'ASPragma',
	#superclass : 'Object',
	#instVars : [
		'args'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'as yet unclassified' }
ASPragma class >> args: pramaArgs [
	^ASPragma new args: pramaArgs 
]

{ #category : 'accessing' }
ASPragma >> args: paramaArgs [
	args := paramaArgs 
]

{ #category : 'accessing' }
ASPragma >> arguments [
	^ args
]

{ #category : 'printing' }
ASPragma >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream nextPut: $(.
	args do: [ :arg | aStream print: arg literal ].
	aStream nextPut: $)
]

{ #category : 'testing' }
ASPragma >> isPrimitive [
^ true
]
