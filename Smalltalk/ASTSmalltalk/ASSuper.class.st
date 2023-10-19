"
I represent a reference to super.

"
Class {
	#name : #ASSuper,
	#superclass : #ASSelf,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASSuper class >> method: aMethod [
	^ self new method: aMethod
]

{ #category : #'instance creation' }
ASSuper class >> method: aMethod superclass: aClass [

	^ self new method: aMethod; superclass: aClass
]

{ #category : #compiling }
ASSuper >> zigWalk: aGenerator [

	self invalidAST:  '"super" in a non-target position'
]

{ #category : #compiling }
ASSuper >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ aGenerator targetClass superclass
]
