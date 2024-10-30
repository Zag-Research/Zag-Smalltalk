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

{ #category : 'instance creation' }
ASCOperations >> ariableReferences: referenced modifies: modified [

	
]

{ #category : 'accessing' }
ASCOperations >> children [
^ OrderedCollection new:0
]

{ #category : 'instance creation' }
ASCOperations >> creates: aStackElement [

	aStackElement location creator: self.
	^ aStackElement
]

{ #category : 'Collection API' }
ASCOperations >> do: ignored [
	" this just exists so that `next` can return something that does nothing for a `do:` message "

	^ nil
]

{ #category : 'testing' }
ASCOperations >> isCallOperation [

	^ false
]

{ #category : 'Collection API' }
ASCOperations >> isEmpty [
	" this just exists so that `next` can return something that does nothing for a `do:` message "

	^ true
]

{ #category : 'accessing' }
ASCOperations >> isSend [
	^ false
]

{ #category : 'block handling' }
ASCOperations >> manifestParameters [

]

{ #category : 'accessing' }
ASCOperations >> nextBasicBlocks [

	^ nil
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
ASCOperations >> variableUsage: aDispatchableBlock [
]

{ #category : 'instance creation' }
ASCOperations >> with: anObject [ 
	^ self subclassResponsibility
]
