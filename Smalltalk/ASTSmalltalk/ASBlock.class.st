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
		'references',
		'fields',
		'includeContext',
		'blockData'
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

{ #category : #adding }
ASBlock >> addClosureData [
	blockData add: #closureData
]

{ #category : #'as yet unclassified' }
ASBlock >> blockIndex: aString [
	^ ((blockData indexOf: aString) - 1) asString
]

{ #category : #accessing }
ASBlock >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
]

{ #category : #'instance creation' }
ASBlock >> buildClosureData: modifiedVariables [

	references do: [ :var |
		(modifiedVariables includes: var name) ifFalse: [
			fields add: var name.
			blockData add: var name ] ]
]

{ #category : #'as yet unclassified' }
ASBlock >> buildClosureDataWithModifiers: modifiedVariables [
	blockData add: #closureData.
	self buildClosureData: modifiedVariables
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

{ #category : #accessing }
ASBlock >> closureData: closureData [
	clsrData := closureData
]

{ #category : #'as yet unclassified' }
ASBlock >> closureIndex: aVar [

	^ ((clsrData indexOf: aVar name) -1) asString
]

{ #category : #'as yet unclassified' }
ASBlock >> filter: zigInstructions [
zigInstructions last = '&e.drop,' ifTrue: [ zigInstructions removeLast ].
	^ zigInstructions
]

{ #category : #accessing }
ASBlock >> includeContext [
	includeContext := true
]

{ #category : #accessing }
ASBlock >> initialize [

	super initialize.
	isNonLocalReturn := true.
	modifies := OrderedCollection new.
	references := OrderedCollection new.
	fields := OrderedCollection new.
	includeContext := false.
	blockData := OrderedCollection new.
	blockData add: #footer.
	blockData add: #method
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
ASBlock >> location: closureName [ 
	^((blockData indexOf: closureName) - 1) asString
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
	| nonLocalReturn |
	nonLocalReturn := body nonLocalReturn: stackVars.
	isNonLocalReturn := nonLocalReturn isNotNil.
	
	^nonLocalReturn
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

{ #category : #visiting }
ASBlock >> variableIndex: var [
	(blockData indexOf: var) = 0 ifFalse: [
		^ ((blockData indexOf: var) - 1) asString ].
	^ ((clsrData indexOf: var name) - 1) asString
]

{ #category : #writing }
ASBlock >> write: codeGenerator with: syms [

	| stackVariables |
	stackVariables := self buildStack.

	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	codeGenerator pushNamespace: selector asSymbol asZigString.
	
	selector := codeGenerator symbolOrVirtual: selector asSymbol asZigString.
	codeGenerator emitMethod: selector numLocals: locals size stackSize: self maxDepth closureSize: 0.

	codeGenerator emitVerifySelector.

	"create dummy context"
	codeGenerator emitPushContext.

	self
		writeBody: syms
		stackVars: stackVariables
		codeGenerator: codeGenerator 
		blocks: (ASBlockVisitor selector: nil).

	codeGenerator pushReturn.
	codeGenerator pushClose
]

{ #category : #writing }
ASBlock >> writeBlock: aStream with: syms [

	| stackVariables |
	stackVariables := self buildStack.
	
	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << selector asZigString << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: selector asZigString
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
