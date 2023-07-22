"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'class',
		'pragmas',
		'blocks'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'tests - instance creation' }
ASMethodNode class >> class: aClass selector: aSymbol arguments: args body: body [

	^ self new
		  class: aClass;
		  selector: aSymbol;
		  arguments: args;
		  body: body
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> class: aClass selector: aSymbol arguments: args locals: locals body: body [

	^ self new
		  class: aClass;
		  selector: aSymbol;
		  arguments: args;
		  locals: locals;
		  body: body;
		  pragmas: OrderedCollection new
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> class: aClass selector: aSymbol body: body [

	^ self new
		  class: aClass;
		  selector: aSymbol;
		  body: body
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> class: aClass selector: aSymbol locals: locals body: body [

	^ self new
		  class: aClass;
		  selector: aSymbol;
		  locals: locals;
		  body: body
]

{ #category : #comparing }
ASMethodNode >> = other [

	^ super = other and: [ 
			  self selector = other selector and: [ 
				  self className = other className ] ]
]

{ #category : #visiting }
ASMethodNode >> acceptVisitor: aVisitor [

	^ aVisitor visitMethodNode: self
]

{ #category : #accessing }
ASMethodNode >> blocks [
	blocks ifNil: [
		blocks := OrderedCollection new.
		body ifNotEmpty: [
			body do: [ :statement | blocks addAll: statement blocks ] ] ].
	"	statement isASSend and: [
					statement args notEmpty and: [
						blocks addAll: (statement args select: [ :arg | arg isASBlock ]) ] ].
				statement isASAssign and: [
					statement expression isASSend and: [
						statement expression args notEmpty and: [
							blocks addAll:
								(statement expression args do: [ :arg | arg isASBlock ]) ] ] ] 
				statement isReturn
				] ] ]."
	^ blocks
]

{ #category : #accessing }
ASMethodNode >> class: anASClassNode [ 
	class := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> className [
	^ class name
]

{ #category : #accessing }
ASMethodNode >> clousreSize [
	^0
]

{ #category : #accessing }
ASMethodNode >> complexBlocks [
	^ self blocks reject: [ :block | block isNonLocalReturn ]
]

{ #category : #accessing }
ASMethodNode >> filter: zigInstructions [
	"nothing to do here"

	^ zigInstructions
]

{ #category : #comparing }
ASMethodNode >> hash [
	^ class hash bitXor: selector hash
]

{ #category : #testing }
ASMethodNode >> isASBlock [
	^false
]

{ #category : #accessing }
ASMethodNode >> maxBlockLocals [
	^0
]

{ #category : #accessing }
ASMethodNode >> maxDepth [

	| argMaxDepth blockLocalMaxDepth |
	argMaxDepth := body
		               ifEmpty: [ 0 ]
		               ifNotEmpty: [
		               body max: [ :statement | statement maxDepth ] ].
	blockLocalMaxDepth := self blocks ifEmpty: [0] ifNotEmpty: [ blocks max: [ :block | block arguments size ] ].
	^ argMaxDepth + blockLocalMaxDepth
]

{ #category : #accessing }
ASMethodNode >> nameSpace [
	^(nestedScopeOf name, '>>', selector) asSymbol asZigString
]

{ #category : #private }
ASMethodNode >> pragmas: somePragmas [ 
	pragmas := somePragmas 
]

{ #category : #printing }
ASMethodNode >> printOn: s [

	s << self class name << ' class: ' << class name
	<< ' selector: '.
	selector printOn: s.
	super printOn: s
]

{ #category : #accessing }
ASMethodNode >> selector [
	^ selector
]

{ #category : #accessing }
ASMethodNode >> selector: aString [ 
	selector := aString
]

{ #category : #utilities }
ASMethodNode >> stackSize [
	^0
]

{ #category : #writing }
ASMethodNode >> writeDispatch: aStream with: smys [ 
	aStream tab.
	aStream << 'try dispatch.addMethod(ClassIndex.' << class name << ', ' << self nameSpace << '.asCompiledMethodPtr());'.
	aStream cr
]

{ #category : #initialization }
ASMethodNode >> writeLiterals: aStream with: syms [
	"check if the method used a placeholder for recursive sends and update that placeholders name with the method name"

	| complexBlocks |
	aStream tab.
	aStream << self nameSpace.
	aStream << '.setLiterals('.
	syms
		associationAt: selector
		ifPresent: [ :sym |
		aStream << ('&[_]Object{sym.' , sym key , '}, ') ]
		ifAbsent: [ aStream << 'empty,' ].

	complexBlocks := self complexBlocks.
	complexBlocks ifEmpty: [ aStream << 'empty' ] ifNotEmpty: [
		aStream << '&[_]Object{'.
		complexBlocks
			do: [ :block | aStream << block selector asZigString ]
			separatedBy: [ aStream << ', ' ].
		aStream << '}' ].

	aStream << ');'.
	aStream cr
]

{ #category : #accessing }
ASMethodNode >> writeMethod: aStream with: syms [

	| stackVariables stackSize statementVisitor blockVisitor |
	"what are the stack variables?"
	stackVariables := OrderedCollection new.

	"set the selector for each block that needs a closure and add it to the stack variables"
	blockVisitor := ASBlockVisitor new.
	blockVisitor visit: body.
	blockVisitor blocks do: [ :b | Transcript show: b selector ].

	self complexBlocks ifNotEmpty: [
		self complexBlocks doWithIndex: [ :block :index |
			block selector: selector , ':' , index asString.
			stackVariables add: block selector asZigString
			"self halt.
			block acceptVisitor: blockVisitor" ] ].

	stackVariables addAll: (locals collect: [ :local | local name ]).
	"capture the size of the stack before arguments are added"
	stackSize := stackVariables size.
	"add the locals, arguments and self"
	stackVariables addAll: (arguments collect: [ :arg | arg name ]).
	stackVariables addLast: #self.

	aStream cr.
	aStream << 'var ' << self nameSpace << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: selector
		 ifPresent: [ :value | value ]
		 ifAbsent: [ 'sym.' , selector asZigString ]) << ','
	<< stackSize asString << ','
	<< (self maxBlockLocals asString , '+' , self clousreSize asString)
	<< ',.{'.
	aStream cr.

	"verify selector or primitive?"
	pragmas
		ifEmpty: [
			aStream tab.
			aStream << '&e.verifySelector,'.
			aStream cr ]
		ifNotEmpty: [
			pragmas do: [ :pragma |
				aStream tab.
				aStream << '&p.p' << (pragma arguments at: 1) value asString
				<< ','.
				aStream cr ] ].

	"create dummy context"
	aStream tab.
	aStream << '&e.pushContext,"^",'.
	aStream cr.

	blockVisitor modifiers isNotEmpty ifTrue: [
		aStream tab.
		aStream
		<<
		'&e.closureData, 3 + (1 << 8), // local:3 size:1 (offset 1 is l1)'.
		aStream cr ].

	"ouput any block closure"
	self complexBlocks ifNotEmpty: [
		self complexBlocks doWithIndex: [ :block :index |
			block isNonLocalReturn ifFalse: [
				aStream tab.
				aStream << (block zigClosureString: stackVariables).
				aStream cr ] ] ].

	self writeBody: syms with: stackVariables and: aStream.

	body ifNotEmpty: [
		body last isReturn ifFalse: [
			aStream tab.
			aStream << '&e.returnTop,' ] ].
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr.

	"ouput compile methods for the blocks"
	self blocks ifNotEmpty: [
		blocks do: [ :block |
			block isNonLocalReturn ifFalse: [
				block writeBlock: aStream with: syms and: stackVariables ] ] ]
]
