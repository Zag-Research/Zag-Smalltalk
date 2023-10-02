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
	#name : #ASTextCodeGenerator,
	#superclass : #ASCodeGenerator,
	#instVars : [
		'mainStream',
		'symbols',
		'label',
		'testing',
		'componentName'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #initialization }
ASTextCodeGenerator >> asString [
	^ mainStream contents
]

{ #category : #printing }
ASTextCodeGenerator >> comma [
	self nextPut: $,
]

{ #category : #accessing }
ASTextCodeGenerator >> comment: aString [
]

{ #category : #completion }
ASTextCodeGenerator >> finished [
]

{ #category : #initialization }
ASTextCodeGenerator >> initialize [

	super initialize.
	mainStream := '' writeStream.
	symbols := OrderedCollection new.
	label := 0.
	testing := false
]

{ #category : #printing }
ASTextCodeGenerator >> lf [

	testing ifFalse: [	mainStream lf]
]

{ #category : #accessing }
ASTextCodeGenerator >> name [

	^ componentName ifNil: [
		  componentName := context name: self prefix: self prefix main: self selector ]
]

{ #category : #printing }
ASTextCodeGenerator >> nextPut: aCharacter [ 
	mainStream nextPut: aCharacter
]

{ #category : #printing }
ASTextCodeGenerator >> nextPutAll: aString [ 
	mainStream nextPutAll: aString
]

{ #category : #accessing }
ASTextCodeGenerator >> prefix [

	^ 'Z'
]

{ #category : #accessing }
ASTextCodeGenerator >> print: anObject [

	mainStream print: anObject
]

{ #category : #accessing }
ASTextCodeGenerator >> selector [
^ ''
]

{ #category : #accessing }
ASTextCodeGenerator >> symbol: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsent: [ name ]
]

{ #category : #accessing }
ASTextCodeGenerator >> symbolOrVirtual: name [

	^ symbols
		  at: name
		  ifPresent: [ :value | value ]
		  ifAbsentPut: [ symbols virtualName ]
]

{ #category : #printing }
ASTextCodeGenerator >> tab [

	testing ifFalse: [ mainStream tab ]
]

{ #category : #printing }
ASTextCodeGenerator >> testing [
	testing := true
]
