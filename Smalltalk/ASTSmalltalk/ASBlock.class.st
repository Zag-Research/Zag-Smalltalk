"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : 'ASBlock',
	#superclass : 'ASMethodOrBlock',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASBlock >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
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

{ #category : 'compiling' }
ASBlock >> scope [
"what are we returning here?"
^nil
]

{ #category : 'accessing' }
ASBlock >> selector [

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ #value ].
	^ (String streamContents: [ :s |
		 size timesRepeat: [ s nextPutAll: 'value:' ] ]) asSymbol
]

{ #category : 'accessing' }
ASBlock >> selectorWithArgs [

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ String empty ].
	^ (String streamContents: [ :s |
		   arguments
			   do: [ :arg |
				   s nextPutAll: 'value: '.
				   s nextPutAll: arg ]
			   separatedBy: [ s space ] ]) asSymbol
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

	self halt.
	aGenerator
		selector: self selector;
		sender: nil locals: locals parameters: arguments.
	^ aGenerator walkBody: [
		  body
			  zigWalkEmpty: [ aGenerator returnLiteral: nil ]
			  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
			  last: [ :exp | exp zigWalkFinal: aGenerator ] ]
]

{ #category : 'compiling' }
ASBlock >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ BlockClosure 
]
