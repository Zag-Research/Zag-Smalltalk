Extension { #name : 'Float' }

{ #category : '*ASTSmalltalk' }
Float >> astImageObject: anASImage [

	^ anASImage objectFloat: self
]

{ #category : '*ASTSmalltalk' }
Float >> astObject: anASImage [ 
	^ self
]
