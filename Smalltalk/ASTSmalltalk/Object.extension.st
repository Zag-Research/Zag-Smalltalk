Extension { #name : #Object }

{ #category : #'*ASTSmalltalk' }
Object >> asASIdentifier [
	(#('true' 'false' 'nil' 'thisContext') includes: self) ifTrue: [ ^ self asASSpecial ].
	(#('self' 'super') includes: self) ifTrue: [ ^ ASSelf new value: self ].
	^ ASIdentifier new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASLiteral [
	^ ASLiteral new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSelf [
	^ ASSelf new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSpecial [
	^ ASSpecial new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASString [
	^ ASString new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSymbol [
	^ ASSymbol new value: self asSymbol
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSyntax [
	^ ASSyntax new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> astObject: anASImage [ 
	^ anASImage generalObject: self
]
