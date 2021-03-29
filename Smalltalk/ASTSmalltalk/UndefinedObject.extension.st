Extension { #name : #UndefinedObject }

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astImageObject: anASImage [ 
	^ anASImage literal: 1 class: 2
]

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astObject: anASImage [ 
	^ self
]
