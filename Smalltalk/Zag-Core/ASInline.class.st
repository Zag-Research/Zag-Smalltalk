"
Am am the abstract class for inliners
"
Class {
	#name : 'ASInline',
	#superclass : 'Object',
	#category : 'Zag-Core-Inlining',
	#package : 'Zag-Core',
	#tag : 'Inlining'
}

{ #category : 'inlining' }
ASInline >> inline: anASCSend with: anASCodeGenerator [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ASInline >> isPrimitive [

	^ false
]
