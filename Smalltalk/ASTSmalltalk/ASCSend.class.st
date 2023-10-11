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
		'target',
		'tailCall'
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

	anOutput visitSend: selector returnIndex: bb index

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
ASCSend >> target: possiblyASymbol [
	target := possiblyASymbol 
]

{ #category : #accessing }
ASCSend >> useCall [
	useCall := true
]

{ #category : #'instance creation' }
ASCSend >> with: aSymbol [

	selector := aSymbol
]
