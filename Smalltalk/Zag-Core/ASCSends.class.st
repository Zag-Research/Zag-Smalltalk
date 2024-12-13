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
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
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

{ #category : 'data flow instructions' }
ASCSends >> markReferences [

	stack markReferenced: selector numArgs
]

{ #category : 'stack' }
ASCSends >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	^ self
		  createsTemporary: (anASStack drop: selector numArgs + 1)
		  data: (AStSubClass new dataClass: Object)
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
