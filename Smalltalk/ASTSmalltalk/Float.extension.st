Extension { #name : #Float }

{ #category : #'*ASTSmalltalk' }
Float >> astImageObject: anASImage [ 
	^ ('16r',self hex) asNumber
]

{ #category : #'*ASTSmalltalk' }
Float >> astObject: anASImage [ 
	^ self
]
