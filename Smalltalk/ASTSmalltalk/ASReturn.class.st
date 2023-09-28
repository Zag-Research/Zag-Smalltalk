"
I am the only non-expression statement type
"
Class {
	#name : #ASReturn,
	#superclass : #ASStatement,
	#instVars : [
		'expression'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASReturn class >> expression: anASNode [
^self new expression: anASNode
]

{ #category : #comparing }
ASReturn >> = other [

	^ self class = other class and: [ self expression = other expression ]
]

{ #category : #visiting }
ASReturn >> acceptVisitor: aVisitor [

	^ aVisitor visitReturn: self
]

{ #category : #converting }
ASReturn >> asZagCompileObject [
	| results |
	results := OrderedCollection new.
	
	results add: 'c.ASReturn,'.
	results add: '":', label,'",'.
	results add: '"',expression asZagLabel,'",'.
	
	results addAll: expression asZagCompileObject.

	^results
]

{ #category : #accessing }
ASReturn >> blocks [
	^expression blocks
]

{ #category : #accessing }
ASReturn >> expression [

	^ expression
]

{ #category : #accessing }
ASReturn >> expression: anASNode [
	expression := anASNode
]

{ #category : #testing }
ASReturn >> isReturn [
	^ true
]

{ #category : #accessing }
ASReturn >> maxDepth [
	^expression maxDepth
]

{ #category : #printing }
ASReturn >> printOn: s [

	s << self className << ' expression: ('.
	expression printOn: s.
	s << ')'
]

{ #category : #building }
ASReturn >> zigString: syms and: stackVariables [
^ expression zigString: syms and: stackVariables , ' &e.returnTop,'
	"expression isASLiteral ifTrue: [
		expression literal isNumber ifTrue: [
			^ '&e.pushLiteral, Object.from(' , expression literal asString
			  , '),' , ' &e.returnTop,' ].
		expression literal ifNil: [ ^ '&e.pushNonlocalBlock_nil,' ].
		expression literal isString ifTrue: [
			^ expression zigString: syms and: stackVariables , ' &e.returnTop,' ].
		expression literal ifFalse: [ ^ '&e.pushNonlocalBlock_false,' ].
		expression literal ifTrue: [ ^ '&e.pushNonlocalBlock_true,' ] ].
	expression isASSend ifTrue: [
		^ expression zigString: syms and: stackVariables , ' &e.returnTop,' ].
	expression isASSelf ifTrue: [
		^ expression zigString: syms and: stackVariables , ' &e.returnTop,' ].
	expression isASRef ifTrue: [
		^ expression zigString: syms and: stackVariables , ' &e.returnTop,' ]"
]
