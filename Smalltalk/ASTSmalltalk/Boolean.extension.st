Extension { #name : #Boolean }

{ #category : #'*ASTSmalltalk' }
Boolean >> astImageObject: anASImage [ 
	^ anASImage literal: self asBit class: 3+self asBit
]

{ #category : #'*ASTSmalltalk' }
Boolean >> astObject: anASImage [ 
	^ self
]
