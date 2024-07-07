"
I remember the selector for sends
"
Class {
	#name : 'ASCSend',
	#superclass : 'ASCEmbed',
	#instVars : [
		'useCall',
		'bb',
		'target'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'initialization' }
ASCSend >> initialize [ 

super initialize .	useCall := false
]

{ #category : 'inlining' }
ASCSend >> inlineIfPossible: anASCodeGenerator [

	(ASInliner lookup: selector stack: stack) ifNotNil: [ : inliner | ^ inliner inline: self with: anASCodeGenerator ].
	self halt
]

{ #category : 'printing' }
ASCSend >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	bb
		ifNotNil: [
			aStream
				<< ' -> ';
				<< bb label ]
		ifNil: [ aStream << ' ( tailcall )' ]
]

{ #category : 'testing' }
ASCSend >> isCallOperation [

	^ true
]

{ #category : 'accessing' }
ASCSend >> isSend [
	^ true
]

{ #category : 'block handling' }
ASCSend >> manifestParameters [

	^ stack makeManifestFor: selector numArgs + 1
]

{ #category : 'accessing' }
ASCSend >> next [

	^ bb
]

{ #category : 'code generation' }
ASCSend >> outputTo: anOutput [

	^ anOutput
		  visitSend: selector
		  returnIndex: bb
		  target: target
]

{ #category : 'testing' }
ASCSend >> requiresContext [
	^ true
]

{ #category : 'handling' }
ASCSend >> return: aBasicBlock [

	bb := aBasicBlock 
]

{ #category : 'accessing' }
ASCSend >> selector [
	^ selector 
]

{ #category : 'accessing' }
ASCSend >> selector: aString [ 
	selector := aString
]

{ #category : 'accessing' }
ASCSend >> target [

	^ target
]

{ #category : 'accessing' }
ASCSend >> target: aSymbol [
	target := aSymbol
]

{ #category : 'accessing' }
ASCSend >> useCall [
	useCall := true
]
