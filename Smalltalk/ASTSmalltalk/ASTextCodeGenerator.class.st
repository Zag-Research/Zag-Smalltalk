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
		'name'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #initialization }
ASTextCodeGenerator >> asString [
	^ mainStream contents
]

{ #category : #'accessing - keys - symbols' }
ASTextCodeGenerator >> comma [
	self nextPut: $,
]

{ #category : #accessing }
ASTextCodeGenerator >> comment: aString [
]

{ #category : #completion }
ASTextCodeGenerator >> finished [
]

{ #category : #completion }
ASTextCodeGenerator >> initialize [

	super initialize.
	mainStream := '' writeStream.
	symbols := OrderedCollection new.
	label := 0
]

{ #category : #accessing }
ASTextCodeGenerator >> lf [

	mainStream lf
]

{ #category : #accessing }
ASTextCodeGenerator >> name [

	^ name ifNil: [
		  name := context name: nil prefix: self prefix main: self selector ]
]

{ #category : #accessing }
ASTextCodeGenerator >> nextPut: aCharacter [ 
	mainStream nextPut: aCharacter
]

{ #category : #accessing }
ASTextCodeGenerator >> nextPutAll: aString [ 
	mainStream nextPutAll: aString
]

{ #category : #accessing }
ASTextCodeGenerator >> prefix [
	^ ''
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
		  ifAbsentPut: [ symbols virutalName ]
]

{ #category : #accessing }
ASTextCodeGenerator >> tab [

	mainStream tab
]
