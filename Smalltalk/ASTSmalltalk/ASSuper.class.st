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
