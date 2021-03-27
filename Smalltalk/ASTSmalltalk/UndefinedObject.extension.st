Extension { #name : #UndefinedObject }

{ #category : #'*ASTSmalltalk' }
UndefinedObject >> astObject: anASImage [ 
	^ anASImage literal: 1 class: 2
]
