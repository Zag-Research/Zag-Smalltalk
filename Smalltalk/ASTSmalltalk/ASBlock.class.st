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
		'isNonLocalReturn'
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
ASBlock >> blocks [

	| myBlocks |
	myBlocks := OrderedCollection new.
	myBlocks add: self.
	self body do: [ :statement | myBlocks addAll: statement blocks ].
	^ myBlocks
]

{ #category : #testing }
ASBlock >> isASBlock [
	^true
]

{ #category : #accessing }
ASBlock >> isNonLocalReturn [
	^ isNonLocalReturn
]

{ #category : #accessing }
ASBlock >> maxDepth [
	^locals size
]

{ #category : #printing }
ASBlock >> printOn: s [

	s << self className.
	super printOn: s
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
		 ifAbsent: [ 'sym.' , selector ]) << ',' << locals size asString
	<< ',' << self maxDepth asString << ',.{'.
	aStream cr.

	aStream tab.
	aStream << '&e.verifySelector,'.
	aStream cr.

	"create dummy context"
	aStream tab.
	aStream << '&e.pushContext,"^",'.
	aStream cr.

	"ouput any blocks"
	"self blocks ifNotEmpty: [
		blocks do: [ :block |
			aStream << (block zigString: syms and: stackVariables) ] ]."

	body do: [ :asStatement |
		aStream tab.
		aStream << (asStatement zigString: syms and: stackVariables).
		aStream cr ].


	aStream tab.
	aStream << '&e.returnTop,'.
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr
]

{ #category : #building }
ASBlock >> zigString: syms and: stackVariables [

	isNonLocalReturn := false.
	(body size == 1 and: (body at: 1) isReturn) ifTrue: [ "this is not a simple local return so need to execute some block"
		| return |
		"return a non local return clousure""tests to see what non local return to return"
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
	"or return a block closure"
	selector
		ifNil: [
			isNonLocalReturn := false.
			^ '&e.blockClosure,"' , nestedScopeOf selector , '",1+(3<<16),' ]
		ifNotNil: [ ^ '&e.blockClosure,"' , selector , '",1+(3<<16),' ]
]
