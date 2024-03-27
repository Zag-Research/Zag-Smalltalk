Extension { #name : #Character }

{ #category : #'*ASTSmalltalk' }
Character >> astImageObject: anASImage [

	^ anASImage putCharacter: self
]

{ #category : #'*ASTSmalltalk' }
Character >> astObject: anASImage [ 
	^ self
]
