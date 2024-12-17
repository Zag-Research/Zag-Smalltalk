"
I represent a set of methods to be inlined not known to be exhaustive.
That is, ther may be other possible implementations or DNUs because the target class couldn't be inferred.
"
Class {
	#name : 'ASInlineNotExhaustive',
	#superclass : 'ASInlineExhaustive',
	#category : 'Zag-Inlining-Inline',
	#package : 'Zag-Inlining',
	#tag : 'Inline'
}

{ #category : 'inlining' }
ASInlineNotExhaustive >> inline: anASCSend with: anASCodeGenerator [

	^ anASCodeGenerator inlineMultiple: methods from: anASCSend exhaustive: false
]
