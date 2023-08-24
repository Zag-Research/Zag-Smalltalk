"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : #ASZigThread,
	#superclass : #ASCodeGenerator,
	#instVars : [
		'stream'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASZigThread >> comment: aString [

	stream
		nextPutAll: ' // '.
	aString printOn: stream
]

{ #category : #accessing }
ASZigThread >> label: aSymbol [

	stream cr;nextPut:$".
	aSymbol printOn: stream.
	stream nextPut:$";nextPut:$,
]

{ #category : #accessing }
ASZigThread >> labelRef: aSymbol [

	stream cr;nextPut:$".
	aSymbol printOn: stream.
	stream nextPut: $";nextPut:$,
]

{ #category : #accessing }
ASZigThread >> object: anObject [

	stream cr.
	anObject printZigOn: stream.
	stream nextPut: $,
]
