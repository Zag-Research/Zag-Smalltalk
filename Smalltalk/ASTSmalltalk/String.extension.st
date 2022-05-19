Extension { #name : #String }

{ #category : #'*ASTSmalltalk' }
String >> asZigName [
	(self allSatisfy: #isAlphaNumeric) ifTrue: [ ^ self asString ].
	^ '@"',self,'"' 
]
