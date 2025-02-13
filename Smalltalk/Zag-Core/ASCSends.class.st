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

{ #category : 'testing' }
ASCSends >> isCallOperation [

	^ true
]

{ #category : 'accessing' }
ASCSends >> isSend [
	^ true
]

{ #category : 'stack' }
ASCSends >> resultStack: anASStack [

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
