"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'pragmas'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'tests - instance creation' }
ASMethodNode class >> selector: aSymbol arguments: args locals: locals body: body [

	^ (self selector: aSymbol locals: locals body: body) arguments: args
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> selector: aSymbol body: body [

	^ self new
		  selector: aSymbol;
		  body: body;
		  pragmas: #(  )
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> selector: aSymbol locals: locals body: body [

	^ (self locals: locals body: body)
		  selector: aSymbol;
		  pragmas: #(  )
]

{ #category : #'tests - instance creation' }
ASMethodNode class >> selector: aSymbol pragmas: pragmas arguments: args locals: locals body: body [

	^ (self arguments: args locals: locals body: body)
		  selector: aSymbol;
		  pragmas: pragmas
]

{ #category : #comparing }
ASMethodNode >> = other [

	^ super = other and: [ 
			  self selector = other selector and: [ 
				  self className = other className ] ]
]

{ #category : #accessing }
ASMethodNode >> closureSize [
	^0
]

{ #category : #comparing }
ASMethodNode >> hash [

	^ selector hash
]

{ #category : #initialization }
ASMethodNode >> initialize [ 
	super initialize .
	pragmas :=  OrderedCollection new
]

{ #category : #accessing }
ASMethodNode >> inspectAST [
	<inspectorPresentationOrder: 33 title: 'ZigAST'>
	^ SpTreeTablePresenter new
		  roots: self roots;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Blocks' evaluated: #contents);
		  yourself
]

{ #category : #accessing }
ASMethodNode >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream nextPut: $(.
	aStream print: selector.
	aStream nextPut: $)
	
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

{ #category : #codegen }
ASMethodNode >> zigWalkMethod: aGenerator [

	aGenerator selector: selector.
	aGenerator
		locals: (locals)
		arguments: (arguments).
	pragmas do: [ :pragma |
		pragma isPrimitive ifTrue: [
			aGenerator primitive: (pragma arguments at: 1) value ] ].
	^ body
		  zigWalkEmpty: [ aGenerator returnSelf ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkMethodReturn: aGenerator ]
]
