Extension { #name : #String }

{ #category : #'*ASTSmalltalk' }
String >> astImagePut: anASImage [ 
	^ anASImage objectString: self
]

{ #category : #'*ASTSmalltalk' }
String >> zigName [

	(self allSatisfy: #isAlphaNumeric) ifTrue: [ ^ self asString ].
	^ '@"' , self , '"'
]
