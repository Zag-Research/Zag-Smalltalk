"
I represent a method, which is the same as a Block except that the return must be explicit
"
Class {
	#name : #ASMethodNode,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'selector',
		'class',
		'isRecursive'
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
ASMethodNode >> class: anASClassNode [ 
	class := anASClassNode
]

{ #category : #accessing }
ASMethodNode >> className [
	^ class name
]

{ #category : #comparing }
ASMethodNode >> hash [
	^ class hash bitXor: selector hash
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

{ #category : #accessing }
ASMethodNode >> writeMethod: aStream with: syms [
	aStream cr.
	"isRecursive ifTrue: [ aStream << 'const ' << selector << '_ = comptime indexSymbol(1);'. aStream cr ]."
	aStream << 'var ' << selector << ' ='. aStream cr.
	aStream tab. aStream << 'compileMethod(sym.' << selector << ',' << locals size << ',' << '2' << ',.{'. aStream cr.
	body do: [ :asStatement | aStream tab.  aStream << (asStatement zigString: syms) ].
	aStream << '});'. aStream cr.
	"isRecursive ifTrue: [ aStream << selector << '.setLiteral(' << selector << '_, sym.' << selector << ');'. aStream cr ]."
]
