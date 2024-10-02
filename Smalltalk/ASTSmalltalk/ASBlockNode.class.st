"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : 'ASBlockNode',
	#superclass : 'ASCodeBlock',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASBlockNode >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	blocks add: self.
	^ blocks
]

{ #category : 'testing' }
ASBlockNode >> isASBlock [
	^true
]

{ #category : 'accessing' }
ASBlockNode >> maxDepth [
	^locals size
]

{ #category : 'printing' }
ASBlockNode >> printOn: s [

	s << self className.
	super printOn: s
]

{ #category : 'compiling' }
ASBlockNode >> scope [
"what are we returning here?"
^nil
]

{ #category : 'accessing' }
ASBlockNode >> selector [

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ #value ].
	^ (String streamContents: [ :s |
		 size timesRepeat: [ s nextPutAll: 'value:' ] ]) asSymbol
]

{ #category : 'accessing' }
ASBlockNode >> selectorWithArgs [

	| size |
	size := arguments size.
	size = 0 ifTrue: [ ^ 'value' ].
	^ String streamContents: [ :s |
		  arguments
			  do: [ :arg |
				  s nextPutAll: 'value: '.
				  s nextPutAll: arg ]
			  separatedBy: [ s space ] ]
]

{ #category : 'compiling' }
ASBlockNode >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toBlock: self
]

{ #category : 'codegen' }
ASBlockNode >> zigWalk: aGenerator [

	^ aGenerator pushBlock: self
]

{ #category : 'compiling' }
ASBlockNode >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ BlockClosure 
]
