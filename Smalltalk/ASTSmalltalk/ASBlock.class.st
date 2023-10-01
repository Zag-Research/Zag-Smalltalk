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

{ #category : #'as yet unclassified' }
ASBlock >> filter: zigInstructions [
zigInstructions last = '&e.drop,' ifTrue: [ zigInstructions removeLast ].
	^ zigInstructions
]

{ #category : #testing }
ASBlock >> isASBlock [
	^true
]

{ #category : #accessing }
ASBlock >> isNonLocalReturn [
	^ body isNonLocalReturn
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

{ #category : #'as yet unclassified' }
ASBlock >> statementVisitor: symbols stackVars: stackVariables [
	^ ASBlockStatementVisitor new
		  symbols: symbols
		  stackVars: stackVariables
		  closure: self
]

{ #category : #compiling }
ASBlock >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : #writing }
ASBlock >> write: codeGenerator with: syms [

	| stackVariables selector | "no idea where selector comes from "
	stackVariables := self buildStack.


	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	codeGenerator pushNamespace: selector asSymbol asZigString.

	selector := codeGenerator symbolOrVirtual:
		            selector asSymbol asZigString.
	codeGenerator
		emitMethod: selector
		numLocals: locals size
		stackSize: self maxDepth
		closureSize: 0.

	codeGenerator emitVerifySelector.

	"create dummy context"
	codeGenerator pushContext.

	self
		writeBody: syms
		stackVars: stackVariables
		codeGenerator: codeGenerator
		blocks: (ASBlockVisitor selector: nil).

	codeGenerator returnTop.
	codeGenerator pushClose
]

{ #category : #writing }
ASBlock >> writeBlock: aStream with: syms [

	| stackVariables |
	stackVariables := self buildStack.

	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << self selector asZigString << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: self selector asZigName
		 ifPresent: [ :value | value ]
		 ifAbsentPut: [ syms virutalName ]) << ',' << locals size asString
	<< ',' << self maxDepth asString << ',.{'.
	aStream cr.

	aStream tab.
	aStream << '&e.verifySelector,'.
	aStream cr.

	"create dummy context"
	aStream tab.
	aStream << '&e.pushContext,"^",'.
	aStream cr.

	self
		writeBody: syms
		stackVars: stackVariables
		writer: aStream
		blocks: (ASBlockVisitor selector: nil).

	aStream tab.
	aStream << '&e.returnTop,'.
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr
]

{ #category : #printing }
ASBlock >> zigClosureString: stackVariables index: index [

	| local |
	"        &e.blockClosure, ""0foo:bar::1"", 1 + (1 << 8) + (0 << 16)   + (3 << 24), // local:1, 1 field, no includeContext, closureData at local3
        &e.blockClosure, ""1foo:bar::2"", 0 + (1 << 8) + (255 << 16) + (3 << 24), // local:0, 1 field, includeContext, closureData at local3
"
	local := ((stackVariables indexOf: selector asZigString) - 1)
		         asString.

	^ '&e.blockClosure,"' , index asString, selector , '", ' , local , ' + ('
	  , fields size asString , '<< 8)' , ' + (' , (includeContext
		   ifTrue: [ '255' ]
		   ifFalse: [ '0' ]) , '<< 16)' , ' + (' , (clsrData ifEmpty: [ '0' ] ifNotEmpty: [ (blockData indexOf: #closureData) asString  ])
	  , '<< 24)'
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

{ #category : #building }
ASBlock >> zigString: syms and: stackVariables [

	self isNonLocalReturn ifTrue: [
		^ self zigNonLocalBlockString: stackVariables ].

	^ '&e.pushLocal,' , (stackVariables indexOf: self selector) asString , ','
]
