"
All the things on the stack with real content
"
Class {
	#name : 'ASStackNonContext',
	#superclass : 'ASStackNonEmpty',
	#instVars : [
		'source'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASStackNonContext >> getNameOn: anASLLMethodOutput [ 
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> id [
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> mergeWith: aCollection [ 
	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ASStackNonContext >> source [
	^ self subclassResponsibility
]

{ #category : 'evaluating' }
ASStackNonContext >> valueClass [

	^ source valueClass
]
