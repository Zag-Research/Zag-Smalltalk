"
I am the superclass of all the CodeGenerator operations
"
Class {
	#name : #ASCOperations,
	#superclass : #Object,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASCOperations class >> with: anObject [
	^ self new with: anObject
]

{ #category : #'instance creation' }
ASCOperations class >> with: anObject and: another [

	^ self new with: anObject and: another
]

{ #category : #initialization }
ASCOperations >> childeren [
^ OrderedCollection new
]

{ #category : #serializing }
ASCOperations >> manifest: aGenerator [


]

{ #category : #'code generation' }
ASCOperations >> outputTo: aCodeEmitter [

	self subclassResponsibility
]

{ #category : #printing }
ASCOperations >> printOn: aStream [
	aStream nextPutAll: self class name
]

{ #category : #'instance creation' }
ASCOperations >> with: anObject [ 
	^ self subclassResponsibility
]
