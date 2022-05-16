Extension { #name : #Object }

{ #category : #'*ASTSmalltalk' }
Object >> asASIdentifier [
	(#('true' 'false' 'nil' 'thisContext') includes: self) ifTrue: [ ^ self asASSpecial ].
	(#('self' 'super') includes: self) ifTrue: [ ^ self asASSelf ].
	'|' = self ifTrue: [ ^ self asASSyntax ].
	^ ASIdentifier with: self asString
]

{ #category : #'*ASTSmalltalk' }
Object >> asASLiteral [
	^ ASLiteralToken new value: self
]

{ #category : #'*ASTSmalltalk' }
Object >> asASSelf [
	^ ASSelfToken new value: self
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
	^ ASSyntax with: self
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
