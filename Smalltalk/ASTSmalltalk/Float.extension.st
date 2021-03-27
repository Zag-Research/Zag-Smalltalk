Extension { #name : #Float }

{ #category : #'*ASTSmalltalk' }
Float >> astObject: anASImage [ 
	^ ('16r',self hex) asNumber
]
