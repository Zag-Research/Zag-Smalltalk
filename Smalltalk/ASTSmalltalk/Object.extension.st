Extension { #name : #Object }

{ #category : #'*ASTSmalltalk' }
Object >> asASLiteral [
	^ ASLiteral new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSyntax [
	^ ASSyntax new value: self
]
