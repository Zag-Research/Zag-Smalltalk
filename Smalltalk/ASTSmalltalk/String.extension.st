Extension { #name : #String }

{ #category : #'*ASTSmalltalk' }
String >> asASIdentifier [
	^ ASIdentifier new value: self
]
