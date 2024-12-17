Extension { #name : 'Object' }

{ #category : '*Zag-Compiler' }
Object >> asASIdentifier [
	(#('true' 'false' 'nil' 'thisContext') includes: self) ifTrue: [ ^ self asASSpecial ].
	(#('self' 'super') includes: self) ifTrue: [ ^ self asASSelf ].
	'|' = self ifTrue: [ ^ self asASSyntax ].
	^ ASIdentifier with: self asString
]

{ #category : '*Zag-Compiler' }
Object >> asASLiteral [
	^ ASLiteralToken new value: self
]

{ #category : '*Zag-Compiler' }
Object >> asASSelf [
	^ ASSelfToken new value: self
]

{ #category : '*Zag-Compiler' }
Object >> asASSpecial [
	^ ASSpecial new value: self
]

{ #category : '*Zag-Compiler' }
Object >> asASString [
	^ ASString new value: self
]

{ #category : '*Zag-Compiler' }
Object >> asASSymbol [
	^ ASSymbol new value: self asSymbol
]

{ #category : '*Zag-Compiler' }
Object >> asASSyntax [
	^ ASSyntax with: self
]
