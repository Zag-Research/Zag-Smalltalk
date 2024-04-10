Extension { #name : 'Character' }

{ #category : '*ASTSmalltalk' }
Character >> astImageObject: anASImage [ 
	^ anASImage literal: self asInteger class: 7
]

{ #category : '*ASTSmalltalk' }
Character >> astObject: anASImage [ 
	^ self
]
