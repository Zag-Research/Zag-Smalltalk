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

{ #category : #initialization }
ASCSend >> initialize [ 

super initialize .	useCall := false
]

{ #category : #'block handling' }
ASCSend >> manifestParameters: aStack [

	| tStack |
	tStack := aStack.
	selector numArgs + 1 timesRepeat: [ 
		tStack top makeManifest.
		tStack := tStack drop ]
]

{ #category : #accessing }
ASCSend >> next [

	^ bb
]

{ #category : #'code generation' }
ASCSend >> outputTo: anOutput [

	anOutput visitSend: selector returnIndex: bb index target: target
]

{ #category : #testing }
ASCSend >> requiresContext [
	^ true
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
