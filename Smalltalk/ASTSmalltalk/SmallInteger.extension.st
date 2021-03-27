Extension { #name : #SmallInteger }

{ #category : #'*ASTSmalltalk' }
SmallInteger >> astObject: anASImage [ 
	^ anASImage literal: self class: 5
]
