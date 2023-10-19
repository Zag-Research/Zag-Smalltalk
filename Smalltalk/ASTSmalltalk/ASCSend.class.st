"
I remember the selector for sends
"
Class {
	#name : #ASCSend,
	#superclass : #ASCOperations,
	#instVars : [
		'selector',
		'useCall',
		'bb',
		'tailCall',
		'target'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCSend >> bb: anASCReturnBlock [ 
	bb := anASCReturnBlock
]

{ #category : #initialization }
ASCSend >> initialize [ 

super initialize .	useCall := false
]

{ #category : #'code generation' }
ASCSend >> outputTo: anOutput [

	anOutput visitSend: selector returnIndex: bb index target: target
]

{ #category : #handling }
ASCSend >> return: aBasicBlock [ 
	bb := aBasicBlock 
]

{ #category : #accessing }
ASCSend >> selector: aString [ 
	selector := aString
]

{ #category : #'accessing - structure variables' }
ASCSend >> tail: aBoolean [ 
	tailCall := aBoolean 
]

{ #category : #accessing }
ASCSend >> target: aSymbol [
	target := aSymbol 
]

{ #category : #accessing }
ASCSend >> useCall [
	useCall := true
]

{ #category : #'instance creation' }
ASCSend >> with: aSymbol [

	selector := aSymbol
]
