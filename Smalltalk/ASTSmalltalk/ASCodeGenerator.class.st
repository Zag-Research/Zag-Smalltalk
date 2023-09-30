"
I am an abstract code generator. My subclasses are used for several purposes.

There are text generators that are used to create text files (currently only in Zig) that can be compiled with the Zag runtime to generate standalone code.

There will be a runtime code generator written in Zig that will generate threaded or JIT code at runtime.
"
Class {
	#name : #ASCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'context',
		'depth',
		'maxDepth'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext add: self
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doCall: numArgs [

	depth := depth - numArgs
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doPop [

	depth := depth - 1
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doPush [ 
	depth := depth + 1.
	maxDepth := maxDepth max: depth

	
]

{ #category : #initialization }
ASCodeGenerator >> initialize [ 
super initialize.
	depth := maxDepth := 0
]

{ #category : #accessing }
ASCodeGenerator >> lookupObject: anObject [
	^ context lookupObject: anObject 
]
