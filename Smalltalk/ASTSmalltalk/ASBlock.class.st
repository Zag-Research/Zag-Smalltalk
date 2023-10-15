"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : #ASBlock,
	#superclass : #ASMethodOrBlock,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
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

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
]

{ #category : #accessing }
ASBlock >> buildStack [
	| stackVariables |
	stackVariables := OrderedCollection new.
	stackVariables add: #self.
	stackVariables addAll: arguments.
	stackVariables addAll: locals.
	^ stackVariables reverse.
]

{ #category : #testing }
ASBlock >> isASBlock [
	^true
]

{ #category : #accessing }
ASBlock >> isNonLocalReturn [
	^ body isNonLocalReturn
]

{ #category : #testing }
ASBlock >> isNonLocalThunk [

	^ body isNonLocalThunk 
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

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ #value ].
	^ (String streamContents: [ :s |
		 size timesRepeat: [ s nextPutAll: 'value:' ] ]) asSymbol
]

{ #category : #compiling }
ASBlock >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : #codegen }
ASBlock >> zigCompileBlock: aGenerator [

	| selector |
	selector := (String streamContents: [ :s |
		             arguments isEmpty
			             ifTrue: [ s nextPutAll: 'value' ]
			             ifFalse: [
			             arguments size timesRepeat: [ s nextPutAll: 'value:' ] ] ])
		            asSymbol.
	aGenerator startMethod: nil selector: selector.
	aGenerator locals: (locals collect: #name) arguments: arguments.
	body zigWalk: aGenerator
]

{ #category : #building }
ASBlock >> zigNonLocalBlockString: stackVariables [

	self flag: #daniel. " use a Dictionary "
	(body size == 1 and: (body at: 1) isReturn) ifTrue: [ "this is a simple block with a local return"
		| return |
		"tests to see if there is a built in literal function"
		return := (body at: 1) expression.
		return isASLiteral ifTrue: [
			| literal |
			literal := return literal.
			literal = true ifTrue: [ ^ '&e.pushNonlocalBlock_true,' ].
			literal = false ifTrue: [ ^ '&e.pushNonlocalBlock_false,' ].
			literal isNil ifTrue: [ ^ '&e.pushNonlocalBlock_nil,' ].
			literal = 1 ifTrue: [ ^ '&e.pushNonlocalBlock_one,' ].
			literal = -1 ifTrue: [ ^ '&e.pushNonlocalBlock_minusOne,' ].
			literal = 0 ifTrue: [ ^ '&e.pushNonlocalBlock_zero,' ].
			literal = 2 ifTrue: [ ^ '&e.pushNonlocalBlock_two,' ] ].
		return isASSelf ifTrue: [
			^ '&e.pushNonlocalBlock_self,'
			  , (stackVariables indexOf: 'self') asString , ',' ] ]
]

{ #category : #building }
ASBlock >> zigPushBlockString: syms and: stackVariables [

	self isNonLocalReturn ifTrue: [
		^ self zigNonLocalBlockString: stackVariables ].

	^ '&e.pushLocal,' , (stackVariables indexOf: self selector) asString , ','
]

{ #category : #codegen }
ASBlock >> zigWalk: aGenerator [

	^ aGenerator pushBlock: self
]

{ #category : #compiling }
ASBlock >> zigWalkTarget: aGenerator [ 
	self zigWalk: aGenerator
]
