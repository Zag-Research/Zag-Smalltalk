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

{ #category : #comparing }
ASSelf >> = other [

	^ self class = other class and: [ 
		  (other method isSymbol ifTrue: [self method name] ifFalse: [self method]) = 
		  (self method isSymbol ifTrue: [other method name] ifFalse: [other method])  and: [ 
			  self superclass = other superclass ] ]
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

	s << self className << ' new method: '.
	method selector printOn: s.
	superclass ifNotNil: [ 
		s << ' superclass: '
		<< superclass name ]
]

{ #category : #accessing }
ASSelf >> superclass [

	^ superclass
]

{ #category : #accessing }
ASSelf >> superclass: aClass [

	^ superclass := aClass
]
