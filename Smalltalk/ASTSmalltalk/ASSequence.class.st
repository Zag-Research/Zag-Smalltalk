Class {
	#name : #ASSequence,
	#superclass : #Object,
	#instVars : [
		'sequence',
		'zagLabel'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASSequence >> = other [
 ^sequence = other sequence
]

{ #category : #comparing }
ASSequence >> acceptVisitor: aStatementVisitor [

	sequence do: [ :asStatement |
		asStatement acceptVisitor: aStatementVisitor.
		asStatement isASSend ifTrue: [ aStatementVisitor drop ] ]
]

{ #category : #comparing }
ASSequence >> addStatement: anASStatement [

	sequence add: anASStatement
]

{ #category : #comparing }
ASSequence >> initialize [ 
sequence := OrderedCollection new.
]

{ #category : #comparing }
ASSequence >> isNonLocalReturn [
	sequence size == 1 ifTrue: [
		| returnStmt |
		returnStmt := sequence at: 1.
		returnStmt isReturn ifTrue: [
			| expression |
			expression := returnStmt expression.
			expression isASLiteral ifTrue: [ ^ expression isSimple ].
			expression isASSelf ifTrue: [ ^ true ] ] ].
	^ false
]

{ #category : #comparing }
ASSequence >> nonLocalReturn: stackVars [
		(sequence size == 1 and: (sequence at: 1) isReturn) ifTrue: [ "this is a simple block with a local return"
		| return |
		"tests to see if there is a built in literal function"
		return := (sequence at: 1) expression.
		return isASLiteral ifTrue: [
			| literal |
			literal := return literal.
			literal = true ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_true,' ].
			literal = false ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_false,' ].
			literal isNil ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_nil,' ].
			literal = 1 ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_one,' ].
			literal = -1 ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_minusOne,' ].
			literal = 0 ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_zero,' ].
			literal = 2 ifTrue: [
				^ '&e.BlockClosure.pushNonlocalBlock_two,' ] ].
		return isASSelf ifTrue: [
			^ '&e.BlockClosure.pushNonlocalBlock_self,'
			  , ((stackVars indexOf: 'self') -1) asString , ',' ] ].
	^ nil
]

{ #category : #comparing }
ASSequence >> printOn: s [

	sequence do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
]

{ #category : #comparing }
ASSequence >> requiresImplicitReturn [
	^ sequence isEmpty or: [sequence last isReturn not]
]

{ #category : #comparing }
ASSequence >> sequence [
^ sequence
]
