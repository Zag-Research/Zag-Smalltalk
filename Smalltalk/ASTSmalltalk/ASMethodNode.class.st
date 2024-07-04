"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : 'ASMethodNode',
	#superclass : 'ASMethodOrBlock',
	#instVars : [
		'selector',
		'pragmas',
		'classContext',
		'args'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'tests - instance creation' }
ASMethodNode class >> selector: aSymbol arguments: args locals: locals body: body [

	^ (self selector: aSymbol locals: locals body: body) arguments: args
]

{ #category : 'tests - instance creation' }
ASMethodNode class >> selector: aSymbol body: body [

	^ self new
		  selector: aSymbol;
		  body: body;
		  pragmas: #(  )
]

{ #category : 'tests - instance creation' }
ASMethodNode class >> selector: aSymbol locals: locals body: body [

	^ (self locals: locals body: body)
		  selector: aSymbol;
		  pragmas: #(  )
]

{ #category : 'tests - instance creation' }
ASMethodNode class >> selector: aSymbol pragmas: pragmas arguments: args locals: locals body: body [

	^ (self arguments: args locals: locals body: body)
		  selector: aSymbol;
		  pragmas: pragmas
]

{ #category : 'comparing' }
ASMethodNode >> = other [

	^ super = other and: [ 
			  self selector = other selector and: [ 
				  self className = other className ] ]
]

{ #category : 'accessing' }
ASMethodNode >> add: anArg [
	args add: anArg
]

{ #category : 'accessing' }
ASMethodNode >> children [

	| c |
	c := OrderedCollection new.
	c addAll: pragmas.
	c addAll: super  children.
	^ c
]

{ #category : 'accessing' }
ASMethodNode >> classContext [
	^classContext
]

{ #category : 'accessing' }
ASMethodNode >> classContext: aClassContext [
	classContext := aClassContext 
]

{ #category : 'accessing' }
ASMethodNode >> closureSize [
	^0
]

{ #category : 'comparing' }
ASMethodNode >> hash [

	^ selector hash
]

{ #category : 'initialization' }
ASMethodNode >> initialize [
	super initialize .
	pragmas := OrderedCollection new
]

{ #category : 'accessing' }
ASMethodNode >> inspectAST [

	<inspectorPresentationOrder: 33 title: 'ZagAST'>
	^ SpTreeTablePresenter new
		  roots: self roots;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn evaluated: #contents);
		  yourself
]

{ #category : 'testing' }
ASMethodNode >> isASBlock [
	^false
]

{ #category : 'accessing' }
ASMethodNode >> maxBlockLocals [
	^0
]

{ #category : 'accessing' }
ASMethodNode >> pragmas [
	^ pragmas 
]

{ #category : 'private' }
ASMethodNode >> pragmas: somePragmas [ 
	pragmas := somePragmas 
]

{ #category : 'printing' }
ASMethodNode >> printOn: s [

	s
		nextPutAll: self class name;
		nextPutAll: ' selector: ';
		print: selector.
	super printOn: s
]

{ #category : 'accessing' }
ASMethodNode >> selector [
	^ selector
]

{ #category : 'accessing' }
ASMethodNode >> selector: aString [ 
	selector := aString
]

{ #category : 'accessing' }
ASMethodNode >> selectorWithArgs [

	selector numArgs = 0 ifTrue: [ ^ selector ].
	^ (String streamContents: [ :s |
		   (selector findTokens: $:) with: arguments do: [ :keyword :arg |
			   s nextPutAll: keyword.
			   s nextPut: $:.
			   s nextPutAll: arg.
			   s space ] ]) asSymbol 
]

{ #category : 'utilities' }
ASMethodNode >> stackSize [
	^0
]

{ #category : 'codegen' }
ASMethodNode >> zigWalkInline: aGenerator [
	self halt.
	"aGenerator selector: selector.
	
	pragmas do: [ :pragma |
		pragma isPrimitive ifTrue: [
			aGenerator primitive: (pragma arguments at: 1) value ] ]."

	"Daniel Good oportunity to rename things on stack!"
	aGenerator locals: locals indirectArguments: arguments.
	^ body
		  zigWalkEmpty: [ aGenerator returnSelf ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkMethodReturn: aGenerator ]
]

{ #category : 'codegen' }
ASMethodNode >> zigWalkInlineMethod: aGenerator [
	"aGenerator selector: selector.
	
	pragmas do: [ :pragma |
		pragma isPrimitive ifTrue: [
			aGenerator primitive: (pragma arguments at: 1) value ] ]."

	"Daniel Good oportunity to rename things on stack!"
	aGenerator pushSelf.
	aGenerator locals: locals indirectParameters: arguments.
	^ body
		  zigWalkEmpty: [ aGenerator returnSelf ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkInlineMethodReturn: aGenerator ]
]

{ #category : 'codegen' }
ASMethodNode >> zigWalkMethod: aGenerator [

	aGenerator selector: selector.
	aGenerator
		locals: (locals)
		parameters: (arguments).
	pragmas do: [ :pragma |
		pragma isPrimitive ifTrue: [
			aGenerator primitive: (pragma arguments at: 1) value ] ].
	^ body
		  zigWalkEmpty: [ aGenerator returnSelf ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkMethodReturn: aGenerator ]
]
