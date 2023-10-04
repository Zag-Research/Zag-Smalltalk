"
I remember the selector for sends
"
Class {
	#name : #ASCSend,
	#superclass : #ASCOperations,
	#instVars : [
		'selector',
		'useCall'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #initialization }
ASCSend >> initialize [ 

super initialize .	useCall := false
]

{ #category : #'code generation' }
ASCSend >> outputTo: anOutput [

	anOutput send: selector useCall: useCall
]

{ #category : #accessing }
ASCSend >> useCall [
	useCall := true
]

{ #category : #'instance creation' }
ASCSend >> with: aSymbol [ 
	selector := aSymbol
]
