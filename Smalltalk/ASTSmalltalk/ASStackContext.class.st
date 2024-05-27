"
I am an actual context... I only exist if a context is required for this method
"
Class {
	#name : 'ASStackContext',
	#superclass : 'ASStackContextPlaceholder',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASStackContext >> from: anASStack [

	rest := anASStack
]

{ #category : 'API' }
ASStackContext >> makeContextRequired [
^ self
]

{ #category : 'API' }
ASStackContext >> offset: anInteger namedContext: blockLocal stack: blockStack [

	^ rest
		  offset: 0
		  namedContext: nil
		  stack: blockLocal
]

{ #category : 'API' }
ASStackContext >> offset: anInteger to: anObject [

	^ rest
		  offset: anInteger 
		  to: anObject
]

{ #category : 'API' }
ASStackContext >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [


	^ rest
		  offset: 0
		  to: anObject
		  context: nil
		  stack: blockLocal
		  other: blockOther
]

{ #category : 'enumerating' }
ASStackContext >> requiredContextDo: aBlock [
^ aBlock value
]
