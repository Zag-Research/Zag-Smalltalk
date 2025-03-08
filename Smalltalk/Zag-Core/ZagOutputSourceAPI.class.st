"
I capture the API of the source object for code translation
"
Class {
	#name : 'ZagOutputSourceAPI',
	#superclass : 'Object',
	#category : 'Zag-Core-Output',
	#package : 'Zag-Core',
	#tag : 'Output'
}

{ #category : 'API' }
ZagOutputSourceAPI >> codeGen: anObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> genFrom: collection [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> methodAddress: anObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> nextAddress [
" returns the next word as a Code address "

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> nextObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> thatAddress [

	^ self subclassResponsibility
]
