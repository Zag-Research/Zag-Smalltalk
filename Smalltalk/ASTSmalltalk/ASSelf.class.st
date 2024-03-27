"
I represent a reference to self or super.

I have a field pointing to the method of the object I represent.

If I represent ""super"" then the reference to superclass will point to the class for the dispatch
"
Class {
	#name : #ASSelf,
	#superclass : #ASExpression,
	#category : 'ASTSmalltalk-AST'
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

	^ self class = other class
]

{ #category : #accessing }
ASSelf >> isASSelf [
	^true
]

{ #category : #testing }
ASSelf >> isImmediateThunk [
^ true
]

{ #category : #testing }
ASSelf >> isNonLocalThunkable [

	^ true
]

{ #category : #printing }
ASSelf >> printOn: s [

	s << self className 
]

{ #category : #compiling }
ASSelf >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toSelf: self
]

{ #category : #compiling }
ASSelf >> zigWalk: anASCodeGenerator [

	^ anASCodeGenerator pushSelf
]

{ #category : #compiling }
ASSelf >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ aGenerator targetClass 
]
