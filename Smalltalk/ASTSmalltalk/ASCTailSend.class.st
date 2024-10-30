"
I remember the selector for sends

I respresent tail calls, which require some stack manipuation before doing the dispatch
"
Class {
	#name : 'ASCTailSend',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'target',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'callgraph' }
ASCTailSend >> dotEdgesOn: aWriteStream from: aBasicBlock [
]

{ #category : 'callgraph' }
ASCTailSend >> dotStringOn: aStream [

	aStream
		<< 'tail send ';
		print: selector
]

{ #category : 'inlining' }
ASCTailSend >> inlineIfPossible: anASCodeGenerator [

	(ASInliner lookup: selector stack: stack) ifNotNil: [ : inliner | ^ inliner inline: self with: anASCodeGenerator ]
]

{ #category : 'printing' }
ASCTailSend >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector;
		<< ' ( tailcall )'
]

{ #category : 'testing' }
ASCTailSend >> isCallOperation [

	^ true
]

{ #category : 'accessing' }
ASCTailSend >> isSend [
	^ true
]

{ #category : 'testing' }
ASCTailSend >> isTailcall [

	^ true
]

{ #category : 'block handling' }
ASCTailSend >> manifestParameters [

	^ stack makeManifestFor: selector numArgs + 1
]

{ #category : 'data flow instructions' }
ASCTailSend >> markReferences [

	stack markReferenced: selector numArgs
]

{ #category : 'accessing' }
ASCTailSend >> nextBasicBlocks [

	^ nil
]

{ #category : 'code generation' }
ASCTailSend >> outputTo: anOutput [

	^ anOutput visitTailSend: selector receiver: target
]

{ #category : 'testing' }
ASCTailSend >> requiresContext [

	self halt.
	^ false
]

{ #category : 'stack' }
ASCTailSend >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	^ (anASStack drop: selector numArgs + 1) pushDataAsTemporary: (AStSubClass new dataClass: Object)
]

{ #category : 'accessing' }
ASCTailSend >> selector [
	^ selector 
]

{ #category : 'accessing' }
ASCTailSend >> selector: aString [

	selector := aString
]

{ #category : 'accessing' }
ASCTailSend >> target [

	^ target
]

{ #category : 'accessing' }
ASCTailSend >> target: aSymbol [
	target := aSymbol
]

{ #category : 'instance creation' }
ASCTailSend >> with: aSymbol [

	selector := aSymbol
]
