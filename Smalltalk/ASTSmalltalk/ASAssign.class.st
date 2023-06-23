"
I represent an assignment expression. This stores the value of an expression in a variable (local, inherited local, instance, class, shared).
"
Class {
	#name : #ASAssign,
	#superclass : #ASExpression,
	#instVars : [
		'expression',
		'variable'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASAssign class >> variable: v expression: e [
^ self new variable: v expression: e
]

{ #category : #comparing }
ASAssign >> = other [

	^ self class = other class and: [ self variable = other variable and: [ self expression = other expression ] ]
]

{ #category : #visiting }
ASAssign >> acceptVisitor: aVisitor [

	^ aVisitor visitAssign: self
]

{ #category : #accessing }
ASAssign >> blocks [
	^expression blocks
]

{ #category : #comparing }
ASAssign >> expression [
	^ expression
]

{ #category : #testing }
ASAssign >> isASAssign [
	^ true
]

{ #category : #comparing }
ASAssign >> maxDepth [
	^expression maxDepth
]

{ #category : #printing }
ASAssign >> printOn: s [

	s << self className << ' variable: '.
	variable printOn: s.
	s << ' expression: ('.
	expression printOn: s.
	s << ')'
]

{ #category : #compiling }
ASAssign >> tell: aCompiler youAreSent: aSymbol [

	^ expression tell: aCompiler youAreSent: aSymbol
]

{ #category : #comparing }
ASAssign >> variable [
	^ variable
]

{ #category : #accessing }
ASAssign >> variable: v expression: e [

	variable := v.
	expression := e
]

{ #category : #accessing }
ASAssign >> zigString: syms and: stackVariables [
	"load value of expression into the variable?"

	| zigString |
	"evaluate the expression"
	zigString := expression isASLiteral
		             ifTrue: [ "can I just load the value directly?"
			             '&e.popLocal, Object.from('
			             , expression literal asString , '),' ]
		             ifFalse: [
		             '&e.popLocal, '
		             , (expression zigString: syms and: stackVariables) ].
	"should the argument just be a lookup?"
	"push the result of the expression into the variable"
	^ zigString , ' &e.pushLocal, '
	  , (stackVariables indexOf: variable name) asString
]
