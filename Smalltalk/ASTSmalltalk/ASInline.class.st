"
Am am the abstract class for inliners
"
Class {
	#name : 'ASInline',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-Inlining',
	#package : 'ASTSmalltalk',
	#tag : 'Inlining'
}

{ #category : 'inlining' }
ASInline >> inline: anASCSend with: anASCodeGenerator [

	^ self subclassResponsibility
]
