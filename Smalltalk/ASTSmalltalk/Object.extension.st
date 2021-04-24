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
Object >> astFields [
	| nVars fields |
	nVars := self class instSize.
	fields := Array new: nVars.
	1 to: nVars do: [ :i | fields at: i put: (self instVarAt: i) ].

]

{ #category : #'*ASTSmalltalk' }
Object >> astFormat [
	^ 1
]

{ #category : #'*ASTSmalltalk' }
Object >> astImageObject: anASImage [ 
	^ anASImage literal: (anASImage generalObject: self) address >> 3 class: 0
]

{ #category : #'*ASTSmalltalk' }
Object >> astObject: anASImage [ 
	^ anASImage generalObject: self
]
