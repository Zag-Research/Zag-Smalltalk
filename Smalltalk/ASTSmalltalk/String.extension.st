Extension { #name : #String }

{ #category : #'*ASTSmalltalk' }
String >> zigName [

	(self allSatisfy: #isAlphaNumeric) ifTrue: [ ^ self asString ].
	^ '@"' , self , '"'
]
