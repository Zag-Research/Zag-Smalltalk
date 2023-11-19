"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : 'ASBlock',
	#superclass : 'ASMethodOrBlock',
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'tests - instance creation' }
ASBlock class >> arguments: args body: body [

	^ self new
		  arguments: args;
		  body: body
]

{ #category : 'tests - instance creation' }
ASBlock class >> arguments: args locals: locals body: body [

	^ self new
		  arguments: args;
		  locals: locals;
		  body: body
]

{ #category : 'tests - instance creation' }
ASBlock class >> locals: locals body: body [

	^ self new
		  locals: locals;
		  body: body
]

{ #category : 'accessing' }
ASBlock >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
]

{ #category : 'accessing' }
ASBlock >> buildStack [
	| stackVariables |
	stackVariables := OrderedCollection new.
	stackVariables add: #self.
	stackVariables addAll: arguments.
	stackVariables addAll: locals.
	^ stackVariables reverse.
]

{ #category : 'testing' }
ASBlock >> isASBlock [
	^true
]

{ #category : 'accessing' }
ASBlock >> maxDepth [
	^locals size
]

{ #category : 'printing' }
ASBlock >> printOn: s [

	s << self className.
	super printOn: s
]

{ #category : 'accessing' }
ASBlock >> selector [

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ #value ].
	^ (String streamContents: [ :s |
		 size timesRepeat: [ s nextPutAll: 'value:' ] ]) asSymbol
]

{ #category : 'compiling' }
ASBlock >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : 'codegen' }
ASBlock >> zigWalk: aGenerator [

	^ aGenerator pushBlock: self
]

{ #category : 'codegen' }
ASBlock >> zigWalkMethod: aGenerator [

	aGenerator
		locals: (locals collect: #name)
		arguments: (arguments collect: #name).
	aGenerator selector: self selector.
	^ body
		  zigWalkEmpty: [ aGenerator returnNil ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkBlockReturn: aGenerator ]
]

{ #category : 'compiling' }
ASBlock >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ BlockClosure 
]
