"
I have the common code for sends
"
Class {
	#name : 'ASCSends',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'target',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'inlining' }
ASCSends >> inlineIfPossible: anASCodeGenerator [

	(ASInliner lookup: selector stack: stack) ifNotNil: [ : inliner | ^ inliner inline: self with: anASCodeGenerator ]
]

{ #category : 'testing' }
ASCSends >> isCallOperation [

	^ true
]

{ #category : 'accessing' }
ASCSends >> isSend [
	^ true
]

{ #category : 'block handling' }
ASCSends >> manifestParameters [

	^ stack makeManifestFor: selector numArgs + 1
]

{ #category : 'data flow instructions' }
ASCSends >> markReferences [

	stack markReferenced: selector numArgs
]

{ #category : 'stack' }
ASCSends >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	^ (anASStack drop: selector numArgs + 1) pushDataAsTemporary: (AStSubClass new dataClass: Object)
]

{ #category : 'accessing' }
ASCSends >> selector [
	^ selector 
]

{ #category : 'accessing' }
ASCSends >> selector: aString [

	selector := aString
]

{ #category : 'accessing' }
ASCSends >> target [

	^ target
]

{ #category : 'accessing' }
ASCSends >> target: aSymbol [
	target := aSymbol
]

{ #category : 'instance creation' }
ASCSends >> with: aSymbol [

	selector := aSymbol
]
