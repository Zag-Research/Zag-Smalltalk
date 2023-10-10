"
I am a parameter for a method.
ASCSelf is a subclass
"
Class {
	#name : #ASCParameter,
	#superclass : #ASCValue,
	#instVars : [
		'refMax',
		'refMin'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASCParameter class >> name: localName offset: offset [

	^ self new name: name offset: offset
]

{ #category : #'state tracking' }
ASCParameter >> referencedAt: depth [

	refMin := depth min: refMin.
	refMax := depth max: refMax
]
