"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASLocalNamed',
	#superclass : 'ASStackNamed',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'operations' }
ASLocalNamed >> mergeWith: aStackElement [

	data := data mergeWith: aStackElement data
]

{ #category : 'data flow instructions' }
ASLocalNamed >> newElementFor: aBasicBlock [

	^ (self replicate: (next newElementFor: aBasicBlock)) data: aBasicBlock classForLocals new
]
