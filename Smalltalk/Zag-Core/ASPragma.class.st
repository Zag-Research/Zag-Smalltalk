"
I contain pragmas for a method - the only one we currently care about is <primitive: 1>
"
Class {
	#name : 'ASPragma',
	#superclass : 'AST',
	#instVars : [
		'args',
		'isPrimitive'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASPragma class >> selector: aSelector args: pramaArgs [

	^ self new selector: aSelector args: pramaArgs
]

{ #category : 'visiting' }
ASPragma >> accept: aVisitor [
	^aVisitor visitPragma: self
]

{ #category : 'accessing' }
ASPragma >> arguments [
	^ args
]

{ #category : 'converting' }
ASPragma >> asPharoAST [
	"Warning: ASPragma seems to be missing the selector that OCPragmaNode wants."
	^OCPragmaNode selector: nil arguments: (args collect: #asPharoAST).
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

	^ isPrimitive 
]

{ #category : 'accessing' }
ASPragma >> selector: aSelector args: paramaArgs [

	args := paramaArgs.
	isPrimitive := (#primitive: = aSelector)
]

{ #category : 'visiting' }
ASPragma >> visitPragma: aVisitor [

	^ aVisitor visitPragma: self
]
