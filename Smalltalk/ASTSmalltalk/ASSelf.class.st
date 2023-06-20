"
I represent a reference to self or super.

I have a field pointing to the method of the object I represent.

If I represent ""super"" then the reference to superclass will point to the class for the dispatch
"
Class {
	#name : #ASSelf,
	#superclass : #ASExpression,
	#instVars : [
		'method',
		'superclass'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASSelf class >> method: aMethod [

	^ self new method: aMethod
]

{ #category : #'instance creation' }
ASSelf class >> method: aMethod superclass: aClass [

	^ self new method: aMethod; superclass: aClass
]

{ #category : #comparing }
ASSelf >> = other [

	^ self class = other class and: [ 
		  (self method isSymbol
			   ifTrue: [ self method ]
			   ifFalse: [ self method selector ]) = (other method isSymbol
			   ifTrue: [ other method ]
			   ifFalse: [ other method selector ]) and: [ 
			  self superclass = other superclass ] ]
]

{ #category : #visiting }
ASSelf >> acceptVisitor: aVisitor [
	^ aVisitor visitSelfNode: self
]

{ #category : #accessing }
ASSelf >> isASSelf [
	^true
]

{ #category : #accessing }
ASSelf >> method [

	^ method
]

{ #category : #accessing }
ASSelf >> method: aMethod [

	method := aMethod
]

{ #category : #printing }
ASSelf >> printOn: s [

	s << self className << ' method: '.
	(method isSymbol ifTrue: [method] ifFalse: [method selector]) printOn: s.
	superclass ifNotNil: [ s << ' superclass: ' << superclass name ]
]

{ #category : #accessing }
ASSelf >> superclass [

	^ superclass
]

{ #category : #accessing }
ASSelf >> superclass: aClass [

	superclass := aClass
]

{ #category : #compiling }
ASSelf >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toSelf: self
]

{ #category : #building }
ASSelf >> zigString: syms [ 
	^'&e.pushTemp, 1,', '&e.send, ', method selector, ','
]
