"
I represent an expression that is a message send.
"
Class {
	#name : #ASSend,
	#superclass : #ASExpression,
	#instVars : [
		'target',
		'selector',
		'args'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASSend class >> selector: aSymbol args: aCollection [

	^ self new selector: aSymbol args: aCollection
]

{ #category : #'instance creation' }
ASSend class >> target: aTarget selector: aSymbol [

	^ self new target: aTarget; selector: aSymbol 
]

{ #category : #'instance creation' }
ASSend class >> target: expression selector: aSymbol args: aCollection [

	^ self new target: expression; selector: aSymbol args: aCollection
]

{ #category : #comparing }
ASSend >> = other [

	^ self class = other class and: [ 
		  self selector = other selector and: [ 
			  self target = other target and: [ self args = other args ] ] ]
]

{ #category : #visiting }
ASSend >> acceptVisitor: aVisitor [

	^ aVisitor visitSend: self
]

{ #category : #parsing }
ASSend >> args [
	^ args
]

{ #category : #accessing }
ASSend >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	args ifNotNil: [ args do: [ :arg | blocks addAll: arg blocks ] ].
	target isASBlock ifTrue: [ blocks addAll: target blocks ].
	^ blocks
]

{ #category : #parsing }
ASSend >> isASSend [
	^true
]

{ #category : #parsing }
ASSend >> maxDepth [
	^selector numArgs
]

{ #category : #printing }
ASSend >> printOn: s [

	s << self className.
	target ifNotNil: [ 
		s << ' target: ('.
		target printOn: s.
		s << ')' ].
	s << ' selector: '.
	selector printOn: s.
	s << ' args: {'.
	args do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : #parsing }
ASSend >> selector [
	^ selector
]

{ #category : #accessing }
ASSend >> selector: aSelector [
	selector := aSelector
]

{ #category : #parsing }
ASSend >> selector: aSymbol args: aCollection [

	selector := aSymbol.
	args := aCollection
]

{ #category : #parsing }
ASSend >> target [

	^ target
]

{ #category : #parsing }
ASSend >> target: expression [
	target := expression
]

{ #category : #codegen }
ASSend >> zigCompileMethod: aGenerator [

	target zigCompileMethod: aGenerator.
	args do: [:arg| arg zigCompileMethod: aGenerator].
	aGenerator send: selector 
]
