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

{ #category : #comparing }
ASMethodNode >> hash [
	^ className hash bitXor: selector hash
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

{ #category : #codegen }
ASMethodNode >> zigWalkMethod: aGenerator [

	aGenerator selector: selector.
	aGenerator
		locals: (locals collect: #name)
		arguments: (arguments collect: #name).
	pragmas do: [ :pragma |
		pragma isPrimitive ifTrue: [
			aGenerator primitive: (pragma arguments at: 1) value ] ].
	^ body
		  zigWalkEmpty: [ aGenerator returnSelf ]
		  rest: [ :exp | exp zigWalkDiscard: aGenerator ]
		  last: [ :exp | exp zigWalkMethodReturn: aGenerator ]
]
