"
I contain a sequence of statements that make up, among other things, the body of a method
"
Class {
	#name : #ASSequence,
	#superclass : #Array,
	#type : #variable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASSequence >> children [
	^ self
]

{ #category : #'filter streaming' }
ASSequence >> contents [
"create a stream to write the AST on and return it as a String"
	| stream |
	stream := WriteStream on: String new.
	self inspectZagASTOn: stream.

	^ stream contents
]

{ #category : #comparing }
ASSequence >> isImmediateThunk [

	^ self size == 1 and: [ self first isImmediateThunk ]
]

{ #category : #comparing }
ASSequence >> isNonLocalReturn [

	^ self size == 1 and: [ self first isNonLocalReturn ]
]

{ #category : #testing }
ASSequence >> isNonLocalThunk [

	^ self size == 1 and: [ self first isNonLocalThunk ]
]

{ #category : #printing }
ASSequence >> printOn: aStream [
	^ self printAsSelfEvaluatingFormOn: aStream
]

{ #category : #comparing }
ASSequence >> requiresImplicitReturn [

	^ self isEmpty or: [ self last isReturn not ]
]

{ #category : #codegen }
ASSequence >> zigWalk: aGenerator [

	^ self do: [ :statement | statement zigWalkDiscard: aGenerator ]
]
