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
ZagOutputSourceAPI >> next [

	^ self subclassResponsibility
]

{ #category : 'API' }
ZagOutputSourceAPI >> thatAddress [

	^ self subclassResponsibility
]
