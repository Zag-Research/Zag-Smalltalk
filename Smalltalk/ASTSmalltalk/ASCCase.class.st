"
I am used for inlining.

I have a map of class, ??? pairs which will translate into a class-dispatch instruction in threaded code.

Originally there was an ASCSend here but the compiler recognized that there were a limited number of implementations, so we could inline the methods.
"
Class {
	#name : 'ASCCase',
	#superclass : 'ASCOperations',
	#instVars : [
		'next'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'block handling' }
ASCCase >> manifestParameters: stack [
]

{ #category : 'accessing' }
ASCCase >> next [

	^ next
]

{ #category : 'code generation' }
ASCCase >> outputTo: output [

	^ self shouldBeImplemented 
]

{ #category : 'instance creation' }
ASCCase >> with: ignored [


]
