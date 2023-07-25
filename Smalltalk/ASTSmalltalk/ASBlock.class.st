"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : #ASBlock,
	#superclass : #ASMethodOrBlock,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#instVars : [
		'method',
		'selector',
		'isNonLocalReturn',
		'modifies',
		'references'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'tests - instance creation' }
ASBlock class >> arguments: args body: body [

	^ self new
		  arguments: args;
		  body: body
]

{ #category : #'tests - instance creation' }
ASBlock class >> arguments: args locals: locals body: body [
	^ self new
		  arguments: args;
		  locals: locals;
		  body: body
]

{ #category : #'tests - instance creation' }
ASBlock class >> locals: locals body: body [

	^ self new
		  locals: locals;
		  body: body
]

{ #category : #compiling }
ASBlock >> acceptVisitor: aVisitor [
	^ aVisitor visitBlockNode: self
]

{ #category : #accessing }
ASBlock >> assignments [
self halt.
body do: [ :instruction | instruction name ]
]

{ #category : #accessing }
ASBlock >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
]

{ #category : #'as yet unclassified' }
ASBlock >> filter [
self halt.
]

{ #category : #'as yet unclassified' }
ASBlock >> filter: zigInstructions [
zigInstructions last = '&e.drop,' ifTrue: [ zigInstructions removeLast ].
	^ zigInstructions
]

{ #category : #accessing }
ASBlock >> initialize [

	super initialize.
	isNonLocalReturn := true.
	modifies := OrderedCollection new.
	references := OrderedCollection new.
]

{ #category : #testing }
ASBlock >> isASBlock [
	^true
]

{ #category : #accessing }
ASBlock >> isNonLocalReturn [
	body size == 1 ifTrue: [
		| returnStmt |
		returnStmt := body at: 1.
		returnStmt isReturn ifTrue: [
			| expression |
			expression := returnStmt expression.
			expression isASLiteral ifTrue: [ ^ expression isSimple ].
			expression isASSelf ifTrue: [ ^ true ] ] ].
	^ false
]

{ #category : #accessing }
ASBlock >> maxDepth [
	^locals size
]

{ #category : #accessing }
ASBlock >> modifiers [
	^ modifies 
]

{ #category : #'as yet unclassified' }
ASBlock >> modifies: anASVariable [
	modifies add: anASVariable
]

{ #category : #accessing }
ASBlock >> nonLocalReturn: stackVars [

	(body size == 1 and: (body at: 1) isReturn) ifTrue: [ "this is a simple block with a local return"
		| return |
		"tests to see if there is a built in literal function"
		return := (body at: 1) expression.
		return isASLiteral ifTrue: [
			| literal |
			literal := return literal.
			literal = true ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_true,' ].
			literal = false ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_false,' ].
			literal isNil ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_nil,' ].
			literal = 1 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_one,' ].
			literal = -1 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_minusOne,' ].
			literal = 0 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_zero,' ].
			literal = 2 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.BlockClosure.pushNonlocalBlock_two,' ] ].
		return isASSelf ifTrue: [
			isNonLocalReturn := true.
			^ '&e.BlockClosure.pushNonlocalBlock_self,'
			  , (stackVars indexOf: 'self') asString , ',' ] ].
	^ nil
]

{ #category : #printing }
ASBlock >> printOn: s [

	s << self className.
	super printOn: s
]

{ #category : #accessing }
ASBlock >> references: anASLocal [ 
	references add: anASLocal
]

{ #category : #accessing }
ASBlock >> selector [
	^ selector
]

{ #category : #accessing }
ASBlock >> selector: aString [ 
	selector := aString
]

{ #category : #compiling }
ASBlock >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : #writing }
ASBlock >> writeBlock: aStream with: syms and: stackVariables [

	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << selector asZigString << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: selector asZigString
		 ifPresent: [ :value | value ]
		 ifAbsent: [ syms virutalName ]) << ',' << locals size asString
	<< ',' << self maxDepth asString << ',.{'.
	aStream cr.

	aStream tab.
	aStream << '&e.verifySelector,'.
	aStream cr.

	"create dummy context"
	aStream tab.
	aStream << '&e.pushContext,"^",'.
	aStream cr.

	self writeBody: syms with: stackVariables and: aStream.

	aStream tab.
	aStream << '&e.returnTop,'.
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr
]

{ #category : #printing }
ASBlock >> zigClosureString: aCollection [

		selector
			ifNil: [
			^ '&e.blockClosure,"' , nestedScopeOf selector , '",1+(3<<16),' ]
			ifNotNil: [ ^ '&e.blockClosure,"' , selector , '",1+(3<<16),' ]
]

{ #category : #building }
ASBlock >> zigNonLocalBlockString: stackVariables [

	isNonLocalReturn := false.
	(body size == 1 and: (body at: 1) isReturn) ifTrue: [ "this is a simple block with a local return"
		| return |
		"tests to see if there is a built in literal function"
		return := (body at: 1) expression.
		return isASLiteral ifTrue: [
			| literal |
			literal := return literal.
			literal = true ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_true,' ].
			literal = false ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_false,' ].
			literal isNil ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_nil,' ].
			literal = 1 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_one,' ].
			literal = -1 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_minusOne,' ].
			literal = 0 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_zero,' ].
			literal = 2 ifTrue: [
				isNonLocalReturn := true.
				^ '&e.pushNonlocalBlock_two,' ] ].
		return isASSelf ifTrue: [
			isNonLocalReturn := true.
			^ '&e.pushNonlocalBlock_self,'
			  , (stackVariables indexOf: 'self') asString , ',' ] ].

]

{ #category : #building }
ASBlock >> zigPushBlockString: syms and: stackVariables [

	isNonLocalReturn ifTrue: [ ^ self zigNonLocalBlockString: stackVariables ].

	^ '&e.pushLocal,' , (stackVariables indexOf: selector) asString , ','
]

{ #category : #building }
ASBlock >> zigString: syms and: stackVariables [

	isNonLocalReturn ifTrue: [ ^self zigNonLocalBlockString: stackVariables  ].

	^ '&e.pushLocal,' , (stackVariables indexOf: selector) asString , ','
]
