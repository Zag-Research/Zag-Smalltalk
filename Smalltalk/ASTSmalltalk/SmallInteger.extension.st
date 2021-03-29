Extension { #name : #SmallInteger }

{ #category : #'*ASTSmalltalk' }
SmallInteger >> astImageObject: anASImage [ 
	^ anASImage literal: self class: 5
]

{ #category : #'*ASTSmalltalk' }
SmallInteger >> astObject: anASImage [ 
	^ self
]
