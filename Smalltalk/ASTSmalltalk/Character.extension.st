Extension { #name : #Character }

{ #category : #'*ASTSmalltalk' }
Character >> astObject: anASImage [ 
	^ anASImage literal: self asInteger class: 7
]
