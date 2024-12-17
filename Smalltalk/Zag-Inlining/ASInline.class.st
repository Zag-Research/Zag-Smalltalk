"
Am am the abstract class for inliners
"
Class {
	#name : 'ASInline',
	#superclass : 'Object',
	#category : 'Zag-Inlining-Inline',
	#package : 'Zag-Inlining',
	#tag : 'Inline'
}

{ #category : 'inlining' }
ASInline >> inline: anASCSend with: anASCodeGenerator [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ASInline >> isPrimitive [

	^ false
]
