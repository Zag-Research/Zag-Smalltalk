"
I am an abstract code outputter.

Known subclasses:
ASEmbeddedThread - generates a CompiledMethod in a running Zag image.
ASEmbeddedJIT - future
ASZigThread - generates Zig threaded code to write to a .zig file.
ASZigCPS - generates Zig CPS code to write to a .zig file


 My subclasses are used for several purposes.

There are text generators that are used to create text files (currently only in Zig) that can be compiled with the Zag runtime to generate standalone code.

There will be a runtime code generator written in Zig that will generate threaded or JIT code at runtime.
"
Class {
	#name : 'ASTextOutput',
	#superclass : 'Object',
	#instVars : [
		'mainStream',
		'symbols',
		'label',
		'testing',
		'componentName',
		'context'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASTextOutput class >> testing [

	^ self new testing: true
]

{ #category : 'converting' }
ASTextOutput >> asString [
	^ mainStream contents
]

{ #category : 'printing' }
ASTextOutput >> comma [

	mainStream nextPut: $,
]

{ #category : 'accessing' }
ASTextOutput >> comment: aString [
]

{ #category : 'accessing' }
ASTextOutput >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext addComponent: self
]

{ #category : 'completion' }
ASTextOutput >> finished [
]

{ #category : 'initialization' }
ASTextOutput >> initialize [

	super initialize.
	mainStream := '' writeStream.
	symbols := OrderedCollection new.
	label := 0.
	testing := false
]

{ #category : 'printing' }
ASTextOutput >> lf [

	testing ifFalse: [	mainStream lf]
]

{ #category : 'accessing' }
ASTextOutput >> name [

	^ componentName ifNil: [
		  componentName := context name: self prefix: self prefix main: self selector ]
]

{ #category : 'printing' }
ASTextOutput >> nextPut: aCharacter [ 
	mainStream nextPut: aCharacter
]

{ #category : 'printing' }
ASTextOutput >> nextPutAll: aString [ 
	mainStream nextPutAll: aString
]

{ #category : 'accessing' }
ASTextOutput >> prefix [

	^ 'Z'
]

{ #category : 'accessing' }
ASTextOutput >> print: anObject [

	mainStream print: anObject
]

{ #category : 'accessing' }
ASTextOutput >> selector [
	self subclassResponsibility 
]

{ #category : 'printing' }
ASTextOutput >> tab [

	testing ifFalse: [ mainStream tab ]
]

{ #category : 'printing' }
ASTextOutput >> testing: aBoolean [

	testing := aBoolean
]
