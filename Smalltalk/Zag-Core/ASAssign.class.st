"
I represent an assignment expression. This stores the value of an expression in a variable (local, parameters, inherited local/parameters, , instance, class, shared).
"
Class {
	#name : 'ASAssign',
	#superclass : 'ASExpression',
	#instVars : [
		'expression',
		'variable'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASAssign class >> variable: v expression: e [
^ self new variable: v expression: e
]

{ #category : 'comparing' }
ASAssign >> = other [

	^ self class = other class and: [ self variable = other variable and: [ self expression = other expression ] ]
]

{ #category : 'accessing' }
ASAssign >> blocks [
	^expression blocks
]

{ #category : 'accessing' }
ASAssign >> children [

	^ OrderedCollection with: expression
]

{ #category : 'comparing' }
ASAssign >> expression [
	^ expression
]

{ #category : 'printing' }
ASAssign >> inspectZagASTOn: s [

	super inspectZagASTOn: s.
	s
		<< $(.
	variable printOn: s.
	s << $)
]

{ #category : 'testing' }
ASAssign >> isASAssign [
	^ true
]

{ #category : 'comparing' }
ASAssign >> maxDepth [
	^expression maxDepth
]

{ #category : 'compiling' }
ASAssign >> tell: aCompiler youAreSent: aSymbol [

	^ expression tell: aCompiler youAreSent: aSymbol
]

{ #category : 'comparing' }
ASAssign >> variable [
	^ variable
]

{ #category : 'accessing' }
ASAssign >> variable: v expression: e [

	variable := v.
	expression := e
]

{ #category : 'compiling' }
ASAssign >> zigWalk: aGenerator [

	expression zigWalk: aGenerator.
	^ aGenerator storeVariable: variable
]

{ #category : 'compiling' }
ASAssign >> zigWalkDiscard: aGenerator [

	expression zigWalk: aGenerator.
	^ aGenerator popVariable: variable
]
