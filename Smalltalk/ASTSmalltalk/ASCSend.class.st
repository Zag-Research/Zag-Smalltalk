"
I remember the selector for sends
"
Class {
	#name : #ASCSend,
	#superclass : #ASCOperations,
	#instVars : [
		'selector',
		'useCall',
		'doBreak'
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

	self shouldNotImplement 
]

{ #category : #'instance creation' }
ASCSend >> with: aSymbol and: flag [

	selector := aSymbol.
	doBreak := flag
]

{ #category : #visitor }
ASCSend >> zigAccept: aVisitor [

useCall ifTrue: [ 	^ aVisitor visitCall: selector doBreak: doBreak ] ifFalse: [	^ aVisitor visitSend: selector doBreak: doBreak]
]
