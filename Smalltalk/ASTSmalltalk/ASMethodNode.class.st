"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : 'ASMethodNode',
	#superclass : 'ASCodeBlock',
	#instVars : [
		'selector',
		'pragmas',
		'methodClass'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'instance creation' }
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

{ #category : 'instance creation' }
ASMethodNode class >> selector: aSymbol pragmas: pragmas arguments: args locals: locals body: body [

	^ (self
		   selector: aSymbol
		   arguments: args
		   locals: locals
		   body: body) pragmas: pragmas
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

	<inspectorPresentationOrder: 33 title: 'ZAST'>
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
ASMethodNode >> methodClass [

	^ methodClass
]

{ #category : 'accessing' }
ASMethodNode >> methodClass: aClass [

	methodClass := aClass
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
