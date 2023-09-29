"
I am a visitor to inline parameters into a method - updating an ASInline object that will replace a send in some code.
"
Class {
	#name : #ASInlineVisitor,
	#superclass : #Object,
	#instVars : [
		'inline',
		'replacements'
	],
	#category : #'ASTSmalltalk-Visitors'
}

{ #category : #initialization }
ASInlineVisitor >> initialize [
	super initialize.
	replacements := Dictionary new
]
