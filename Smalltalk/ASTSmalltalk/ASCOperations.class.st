"
I am the superclass of all the CodeGenerator operations
"
Class {
	#name : 'ASCOperations',
	#superclass : 'Object',
	#instVars : [
		'stack'
	],
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

{ #category : 'as yet unclassified' }
ASCOperations >> createsTemporary: aStack data: aStackValue [

	^ self creates: ((ASStackTemporary fromStack: aStack) data: aStackValue)
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

{ #category : 'testing' }
ASCOperations >> needsContext: aStackContext [

	^ false
]

{ #category : 'testing' }
ASCOperations >> needsLocals [

	^ false
]

{ #category : 'accessing' }
ASCOperations >> nextBasicBlocks [

	^ nil
]

{ #category : 'code generation' }
ASCOperations >> outputTo: aCodeEmitter [

	self subclassResponsibility
]

{ #category : 'stack' }
ASCOperations >> resultStack: anASStack basicBlock: aBasicBlock [

	^ self subclassResponsibility
]

{ #category : 'initialization' }
ASCOperations >> setInContext: aBoolean [
]

{ #category : 'as yet unclassified' }
ASCOperations >> shrinksStack [

	^ false
]

{ #category : 'Collection API' }
ASCOperations >> stack [

	^ stack
]

{ #category : 'instance creation' }
ASCOperations >> variableReferences: aCollection modifies: aCollection2 block: aBasicBlock [ 
]

{ #category : 'instance creation' }
ASCOperations >> variableUsage: aDispatchableBlock [
]

{ #category : 'instance creation' }
ASCOperations >> with: anObject [ 
	^ self subclassResponsibility
]
