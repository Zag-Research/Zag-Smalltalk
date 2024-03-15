Extension { #name : 'Float' }

{ #category : '*ASTSmalltalk' }
Float >> astImageObject: anASImage [

	^ anASImage putFloat: self
]

{ #category : '*ASTSmalltalk' }
Float >> astObject: anASImage [ 
	^ self
]
