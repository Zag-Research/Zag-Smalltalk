"
I am an abstract code generator.

An instance of one of my subclasses is passed to an AST `emit:` method to generate the actual code for a method.
I know nothing about the structure of an AST, I just generate some form of code from the calls that are made to me.

Known subclasses:
ASEmbeddedThread - generates a CompiledMethod in a running Zag image.
ASEmbeddedJIT - future
ASZigThread - generates Zig threaded code to write to a .zig file.
ASZigCPS - generates Zig CPS code to write to a .zig file

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.


    Implementation Points
"
Class {
	#name : #ASCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'depth',
		'maxDepth'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCodeGenerator >> comment: aString [
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

{ #category : #completion }
ASCodeGenerator >> finished [
]

{ #category : #completion }
ASCodeGenerator >> initialize [ 
	super initialize.
	depth := maxDepth := 0

	
]

{ #category : #accessing }
ASCodeGenerator >> label: aSymbol [

	self subclassResponsibility
]

{ #category : #accessing }
ASCodeGenerator >> labelRef: aSymbol [

	self subclassResponsibility
]

{ #category : #accessing }
ASCodeGenerator >> object: anObject [
	self subclassResponsibility
]
