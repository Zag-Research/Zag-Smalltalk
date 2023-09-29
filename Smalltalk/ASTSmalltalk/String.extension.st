Extension { #name : #String }

{ #category : #'*ASTSmalltalk' }
String >> asZigName [
	(self allSatisfy: #isAlphaNumeric) ifTrue: [ ^ self asString ].
	^ '@"',self,'"' 
]

{ #category : #'*ASTSmalltalk' }
String >> zigName [

	^ ((self includes: $:) or: [ self first isUppercase ])
		  ifFalse: [self]
		  ifTrue: [ '@"' , self , '"' ]
]
