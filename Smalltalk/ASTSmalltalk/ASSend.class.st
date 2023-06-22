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
	args ifNotEmpty: [
		blocks addAll: (args select: [ :arg | arg isASBlock ]) ].
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

{ #category : #building }
ASSend >> zigString: syms [
	syms at: selector ifPresent:[ :sym | ^'&e.send, ', sym ,',' ]
	ifAbsent: [ ^'&e.send, ', selector ,',' ]
	"what about the argumets? Do I assume they are on the stack? Do I need to create a stack frame for the send?"
]
