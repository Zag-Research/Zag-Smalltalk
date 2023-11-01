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

{ #category : #'*ASTSmalltalk' }
Object >> contents [

	| stream |
	stream := WriteStream on: String new.
	self inspectStringOn: stream.

	^ stream contents
]

{ #category : #'*ASTSmalltalk' }
Object >> inspectSlotsOn: aStream [
	self class allSlots
	do:
		[ :aSlot |
			aStream
				nextPutAll: aSlot name;
				nextPutAll: ' => '.
			(self instVarNamed: aSlot name)
				ifNotNil: [ :item | aStream nextPutAll: item asString]
				ifNil: [ aStream nextPutAll: #nil ] ]
		separatedBy: [ aStream space ].
]

{ #category : #'*ASTSmalltalk' }
Object >> zigCompileObject: aGenerator [

	| slots label |
	slots := (self isArray
		          ifTrue: [ self ]
		          ifFalse: [ self class allSlots ]) size.
	
	((1 to: slots) collect: [ :n |
		 | x |
		 x := self instVarAt: n.
		 x := x -> (x zigCompileObjectReference: aGenerator) ]) do: [ :each |
		each value
			ifNil: [ each key zigCompileObject: aGenerator ]
			ifNotNil: [ aGenerator labelRef: each value ].
		aGenerator comma ].
	label := aGenerator nextLabel.
	aGenerator putClass: self class name.
	^ label
]

{ #category : #'*ASTSmalltalk' }
Object >> zigCompileObjectReference: aGenerator [

	(aGenerator lookupObject: self) ifNotNil: [ :v | ^ v ].
	^ self zigCompileObject: aGenerator
]
