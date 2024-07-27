"
I represent an exhaustive set of methods to be inlined.
That is, I know that there aren't any other possible implementations or DNUs because the target class is fully inferred.
"
Class {
	#name : 'ASInlineExhaustive',
	#superclass : 'ASInline',
	#instVars : [
		'methods'
	],
	#category : 'ASTSmalltalk-Inlining',
	#package : 'ASTSmalltalk',
	#tag : 'Inlining'
}

{ #category : 'accessing' }
ASInlineExhaustive >> methods [

	^ methods
]

{ #category : 'accessing' }
ASInlineExhaustive >> methods: aCollection [ 
	methods := aCollection
]

{ #category : 'testing' }
ASInlineExhaustive >> validate: aStackElement [

	^ self
]
