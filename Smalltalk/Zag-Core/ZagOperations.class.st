"
I am the superclass of all the CodeGenerator operations
"
Class {
	#name : 'ZagOperations',
	#superclass : 'Object',
	#instVars : [
		'stack'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'instance creation' }
ZagOperations class >> with: anObject [
	^ self new with: anObject
]

{ #category : 'instance creation' }
ZagOperations class >> with: anObject and: another [

	^ self new with: anObject and: another
]

{ #category : 'accessing' }
ZagOperations >> children [
^ OrderedCollection new:0
]

{ #category : 'instance creation' }
ZagOperations >> creates: aStackElement [

	aStackElement location creator: self.
	^ aStackElement
]

{ #category : 'as yet unclassified' }
ZagOperations >> createsTemporary: aStack data: aStackValue [

	^ self creates: ((ASStackTemporary fromStack: aStack) data: aStackValue)
]

{ #category : 'Collection API' }
ZagOperations >> do: ignored [
	" this just exists so that `next` can return something that does nothing for a `do:` message "

	^ nil
]

{ #category : 'testing' }
ZagOperations >> isCallOperation [

	^ false
]

{ #category : 'Collection API' }
ZagOperations >> isEmpty [
	" this just exists so that `next` can return something that does nothing for a `do:` message "

	^ true
]

{ #category : 'accessing' }
ZagOperations >> isSend [
	^ false
]

{ #category : 'testing' }
ZagOperations >> needsContext: aStackContext [

	^ false
]

{ #category : 'testing' }
ZagOperations >> needsLocals [

	^ false
]

{ #category : 'accessing' }
ZagOperations >> nextBasicBlocks [

	^ nil
]

{ #category : 'code generation' }
ZagOperations >> outputTo: aCodeEmitter [

	self subclassResponsibility
]

{ #category : 'stack' }
ZagOperations >> resultStack: anASStack basicBlock: aBasicBlock [

	^ self subclassResponsibility
]

{ #category : 'initialization' }
ZagOperations >> setInContext: aBoolean [
]

{ #category : 'as yet unclassified' }
ZagOperations >> shrinksStack [

	^ false
]

{ #category : 'Collection API' }
ZagOperations >> stack [

	^ stack
]

{ #category : 'finalAnalysis' }
ZagOperations >> variableReferences: aCollection modifies: aCollection2 block: aBasicBlock [ 
]

{ #category : 'instance creation' }
ZagOperations >> variableUsage: aDispatchableBlock [
]

{ #category : 'instance creation' }
ZagOperations >> with: anObject [ 
	^ self subclassResponsibility
]
