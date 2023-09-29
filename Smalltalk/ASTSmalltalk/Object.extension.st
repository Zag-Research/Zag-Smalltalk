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
Object >> asZig: aGenerator [

	| slots i label |
	slots := self class slots size.
	((1 to: slots) collect: [ :n |
		 | x |
		 x := self instVarAt: n.
		 x -> (x asZigReference: aGenerator) ]) do: [ :each |
		each value
			ifNil: [ each key asZig: aGenerator ]
			ifNotNil: [ aGenerator labelRef: each value ].
		aGenerator comma ].
	label := aGenerator nextLabel.
	aGenerator putClass: self class name.
	^ label
]

{ #category : #'*ASTSmalltalk' }
Object >> asZigReference: anASZigCompileObjectGenerator [

	^ self asZig: anASZigCompileObjectGenerator 
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
