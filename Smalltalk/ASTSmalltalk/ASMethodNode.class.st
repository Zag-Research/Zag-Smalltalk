"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'class',
		'isRecursive',
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
ASMethodNode class >> class: aClass selector: aSymbol  arguments: args locals: locals body: body [
^ self new class: aClass; selector: aSymbol;  arguments: args; locals: locals; body: body
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

{ #category : #comparing }
ASMethodNode >> hash [
	^ class hash bitXor: selector hash
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
ASMethodNode >> numLocals [
	^ (arguments size + locals size) asString
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

{ #category : #initialization }
ASMethodNode >> setLiterals: aZnCharacterWriteStream [ 
	"check if the method used a placeholder for recursive sends and update that placeholders name with the method name"
]

{ #category : #utilities }
ASMethodNode >> stackSize [
	^0
]

{ #category : #accessing }
ASMethodNode >> writeMethod: aStream with: syms [

	| stackVariables |
	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << selector asZigString << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: selector
		 ifPresent: [ :value | value ]
		 ifAbsent: [ 'sym.' , selector asZigString ]) << ','
	<< self stackSize asString << ','
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

	"output instructions"
	stackVariables := scope keys asOrderedCollection.
	stackVariables addLast: #self.

	"ouput any blocks"
	self blocks ifNotEmpty: [
		blocks doWithIndex: [ :block :index |
			block selector: selector , ':' , index asString.
			aStream << (block zigString: syms and: stackVariables) ] ].

	body do: [ :asStatement |
		aStream tab.
		aStream << (asStatement zigString: syms and: stackVariables).
		aStream cr ].


	aStream tab.
	aStream << '&e.returnTop,'.
	aStream cr. "should this be hardcoded?"
	aStream << '});'.
	aStream cr.
	"isRecursive ifTrue: [ aStream << selector << '.setLiteral(' << selector << '_, sym.' << selector << ');'. aStream cr ]."

	"ouput compile methods for the blocks"
	self blocks ifNotEmpty: [
		blocks do: [ :block | block isNonLocalReturn ifFalse: [ 
			block writeBlock: aStream with: syms and: stackVariables ] ] ]
]
