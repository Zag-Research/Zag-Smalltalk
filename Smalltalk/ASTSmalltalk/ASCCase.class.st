"
I am used for inlining.

I have a map of class, ??? pairs which will translate into a class-dispatch instruction in threaded code.

Originally there was an ASCSend here but the compiler recognized that there were a limited number of implementations, so we could inline the methods.
"
Class {
	#name : 'ASCCase',
	#superclass : 'ASCOperations',
	#instVars : [
		'next',
		'map',
		'complete'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCCase >> at: key put: value [

	^ map at: key put: value
]

{ #category : 'accessing' }
ASCCase >> blocks [

	^ map values
]

{ #category : 'accessing' }
ASCCase >> complete: aBoolean [ 
	complete := aBoolean
]

{ #category : 'initialization' }
ASCCase >> initialize [

	super initialize.
	map := Dictionary new.
	complete := false
]

{ #category : 'printing' }
ASCCase >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream space.
	map printElementsOn: aStream
]

{ #category : 'block handling' }
ASCCase >> manifestParameters: stack [
]

{ #category : 'accessing' }
ASCCase >> next [

	^ next
]

{ #category : 'code generation' }
ASCCase >> outputTo: anOutput [
	^ anOutput
		  visitCase: map
]

{ #category : 'instance creation' }
ASCCase >> with: ignored [


]
