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

{ #category : #comparing }
ASSequence >> requiresImplicitReturn [

	^ self isEmpty or: [ self last isReturn not ]
]

{ #category : #codegen }
ASSequence >> zigWalk: aGenerator [

	^ self do: [ :statement | statement zigWalk: aGenerator ]
]
