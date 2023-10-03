"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'className',
		'pragmas'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'tests - instance creation' }
ASMethodNode class >> class: aClass selector: aSymbol arguments: args body: body [
self halt.
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
ASMethodNode >> class: anASClassNode [

	"class := anASClassNode"
]

{ #category : #introspection }
ASMethodNode >> className [

	^ className
]

{ #category : #accessing }
ASMethodNode >> closureSize [
	^0
]

{ #category : #accessing }
ASMethodNode >> filter: zigInstructions [
	"nothing to do here"

	^ zigInstructions
]

{ #category : #comparing }
ASMethodNode >> hash [
	^ className hash bitXor: selector hash
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
ASMethodNode >> nameSpace [
	^(nestedScopeOf name, '>>', selector) asSymbol asZigString
]

{ #category : #accessing }
ASMethodNode >> pragmas [
	^ pragmas 
]

{ #category : #private }
ASMethodNode >> pragmas: somePragmas [ 
	pragmas := somePragmas 
]

{ #category : #printing }
ASMethodNode >> printOn: s [

	s
		nextPutAll: self class name;
		nextPutAll: ' class: ';
		nextPutAll: (className ifNil: [ '?' ]);
		nextPutAll: ' selector: ';
		print: selector.
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

{ #category : #comparing }
ASMethodNode >> statementVisitor: symbols stackVars: stackVariables [ 
	^ ASMethodStatementVisitor new
		  symbols: symbols
		  stackVars: stackVariables
		  closure: self
]

{ #category : #visiting }
ASMethodNode >> variableIndex: anASVar [
	^ (clsrData indexOf: anASVar name) asString.
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

	complexBlocks := OrderedCollection new.
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

	| stackVariables stackSize blockVisitor |
	"what are the stack variables?"
	stackVariables := OrderedCollection new.

	"set the selector for each block that needs a closure and add it to the stack variables"
	blockVisitor := ASBlockVisitor selector: selector.
	blockVisitor visit: body.

	self buildClosureData: blockVisitor modifiers.
	blockVisitor blocks do: [ :b | b closureData: clsrData ].

	stackVariables addAll: blockVisitor closures.
	stackVariables addAll:
		((blockVisitor filterVars: locals) collect: [ :local | local name ]).

	"capture the size of the stack before arguments are added"
	stackSize := stackVariables size.

	"add the locals, arguments and self"
	stackVariables addAll:
		(arguments collect: [ :arg | arg name ]) reverse.
	stackVariables addLast: #self.

	aStream cr.
	aStream << 'var ' << self nameSpace << ' ='.
	aStream cr.
	aStream tab.

	aStream << 'compileMethod(' << (syms
		 at: selector
		 ifPresent: [ :value | value ]
		 ifAbsentPut: [ syms virutalName ]) << ',' << stackSize asString
	<< ','
	<< (self maxBlockLocals asString , '+' , self closureSize asString)
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

	blockVisitor writeClosureData: aStream stackVariables: stackVariables.

	"ouput any block closure"
	blockVisitor blocks doWithIndex: [ :block :index |
		aStream tab.
		aStream
		<<
		(block zigClosureString: stackVariables index: (index - 1) asString).
		aStream cr ].

	"write the instructions"
	self
		writeBody: syms
		stackVars: stackVariables
		writer: aStream
		blocks: blockVisitor.

	"write out the implicit return"
	body requiresImplicitReturn ifTrue: [
		aStream tab.
		aStream << '&e.returnTop,' ].
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr.

	"ouput compile methods for the blocks"
	blockVisitor blocks do: [ :block |
		block isNonLocalReturn ifFalse: [
			block writeBlock: aStream with: syms] ]
]

{ #category : #codegen }
ASMethodNode >> zigCompileMethod: aGenerator [

	| primitive |
	aGenerator selector: selector.
	aGenerator locals: locals arguments: arguments.
	primitive := 0.
	pragmas do: [ :pragma | 
		pragma isPrimitive ifTrue: [
			primitive := (pragma arguments at: 1) value ] ].
	aGenerator primitiveOrVerify: primitive.
	
	self flag: #daniel " most code here can come from writeMethod:with: ".
	
	body zigCompileMethod: aGenerator.
]
