Extension { #name : #Boolean }

{ #category : #'*ASTSmalltalk' }
Boolean >> astObject: anASImage [ 
	^ anASImage literal: self asBit class: 3+self asBit
]
