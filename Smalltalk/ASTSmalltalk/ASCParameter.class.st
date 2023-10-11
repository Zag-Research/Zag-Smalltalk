"
I am a parameter for a method.
ASCSelf is a subclass
"
Class {
	#name : #ASCParameter,
	#superclass : #ASCValue,
	#instVars : [
		'refMax',
		'refMin',
		'offset',
		'name'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASCParameter class >> name: localName offset: offset [

	^ self new name: localName offset: offset
]

{ #category : #initialization }
ASCParameter >> initialize [ 
super initialize .
refMax := refMin := 0
]

{ #category : #accessing }
ASCParameter >> name [ 
^ name
]

{ #category : #'instance creation' }
ASCParameter >> name: aString offset: anInteger [ 
	name := aString .
	offset := anInteger 
]

{ #category : #accessing }
ASCParameter >> offset [ 
^ offset
]

{ #category : #'state tracking' }
ASCParameter >> referencedAt: depth [

	refMin := depth min: refMin.
	refMax := depth max: refMax
]
