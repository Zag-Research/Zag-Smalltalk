"
I am the superclass of all the CodeGenerator operations
"
Class {
	#name : 'ASCOperations',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCOperations class >> with: anObject [
	^ self new with: anObject
]

{ #category : 'instance creation' }
ASCOperations class >> with: anObject and: another [

	^ self new with: anObject and: another
]

{ #category : 'accessing' }
ASCOperations >> children [
^ OrderedCollection new:0
]

{ #category : 'instance creation' }
ASCOperations >> created: aStackElement [

	aStackElement location creator: self.
	^ aStackElement
]

{ #category : 'testing' }
ASCOperations >> isCallOperation [

	^ false
]

{ #category : 'accessing' }
ASCOperations >> isSend [
	^ false
]

{ #category : 'block handling' }
ASCOperations >> manifestParameters [

]

{ #category : 'code generation' }
ASCOperations >> outputTo: aCodeEmitter [

	self subclassResponsibility
]

{ #category : 'testing' }
ASCOperations >> requiresContext [
	^ false
]

{ #category : 'instance creation' }
ASCOperations >> with: anObject [ 
	^ self subclassResponsibility
]
