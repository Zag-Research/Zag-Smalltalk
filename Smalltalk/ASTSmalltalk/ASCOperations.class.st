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

{ #category : #accessing }
ASCOperations >> children [
^ OrderedCollection new
]

{ #category : #printing }
ASCOperations >> contents [

	| stream |
	stream := WriteStream on: String new.
	self inspectStringOn: stream.

	^ stream contents
]

{ #category : #printing }
ASCOperations >> inspectStringOn: aStream [

	aStream nextPutAll: self class name
]

{ #category : #serializing }
ASCOperations >> manifest: aGenerator [


]

{ #category : #'code generation' }
ASCOperations >> outputTo: aCodeEmitter [

	self subclassResponsibility
]

{ #category : #'instance creation' }
ASCOperations >> with: anObject [ 
	^ self subclassResponsibility
]