"
I am a parameter for a method.
ASCSelf is a subclass
"
Class {
	#name : 'ASCParameter',
	#superclass : 'ASCValue',
	#instVars : [
		'offset',
		'stack'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance initialization' }
ASCParameter >> initialize [ 
	^ super initialize 
]

{ #category : 'testing' }
ASCParameter >> isImmediateThunk [
^ true
]

{ #category : 'accessing' }
ASCParameter >> offset [

	^ stack offsetTo: self name
]

{ #category : 'affecting' }
ASCParameter >> push: output [

	^ output visitPushLocal: self
]
