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
		'method'
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

{ #category : #testing }
ASBlock >> isASBlock [
	^true
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

{ #category : #compiling }
ASBlock >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : #writing }
ASBlock >> writeBlock: aStream with: syms selector: selector and: stackVariables [
	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << selector asZigString << ' ='.
	aStream cr.
	aStream tab.
	aStream << 'compileMethod(' << (syms
		 at: selector
		 ifPresent: [ :value | value ]
		 ifAbsent: [ 'sym.' , selector asZigString ]) << ','
	<< locals size asString << ',' << self maxDepth asString << ',.{'.
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
	aStream cr.
]

{ #category : #building }
ASBlock >> zigString: syms and: stackVariables [

	(body size == 1 and: (body at: 1) isReturn)
		ifTrue: [ "return a non local return clousure"
		^ '&e.nonlocalClosure_self,2,' ]
		ifFalse: [ "or return a clock closure"
			^ '&e.blockClosure,"0foo:bar:1",1+(3<<16),' ]
]
