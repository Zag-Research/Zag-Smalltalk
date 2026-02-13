Class {
	#name : 'ZagSmalltalkScannerTest',
	#superclass : 'TestCase',
	#category : 'Zag-Compiler-Tests-Tests',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testAsArray [

	self
		assert: (ZagSmalltalkScanner scan: ' simple') asArray
		equals: { (ASIdentifierToken with: 'simple') }.
	self
		assert: (ZagSmalltalkScanner scan: ' simple 
	^ 42 ') asArray
		equals: {
				(ASIdentifierToken with: 'simple').
				(ASSyntax with: '^').
				42 asASLiteral }.
	self
		assert: (ZagSmalltalkScanner scan: ' simple
	^ 42') asArray
		equals: {
				(ASIdentifierToken with: 'simple').
				(ASSyntax with: '^').
				42 asASLiteral }.
	self
		assert: (ZagSmalltalkScanner scan: '== simple
	^ 42') asArray
		equals: {
				(ASIdentifierToken with: '==').
				(ASIdentifierToken with: 'simple').
				(ASSyntax with: '^').
				42 asASLiteral }
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testAsStream [

	| scanner |
	scanner := ZagSmalltalkScanner scan: ' simple 
	^ 42 '.
	self assert: scanner peek equals: (ASIdentifierToken with: 'simple').
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			(ASIdentifierToken with: 'simple').
			(ASSyntax with: '^').
			42 asASLiteral }.
	self assert: scanner atEnd.
	scanner := ZagSmalltalkScanner scan: ' simple 
	^ 42 '.
	self deny: scanner atEnd.
	self assert: scanner next equals: (ASIdentifierToken with: 'simple').
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			(ASSyntax with: '^').
			42 asASLiteral }
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testIDs [

	self
		assert: (ZagSmalltalkScanner scan: ' abc ') token
		equals: (ASIdentifierToken with: 'abc').
	self
		assert: (ZagSmalltalkScanner scan: ' * ') token
		equals: (ASIdentifierToken with: '*').
	self
		assert: (ZagSmalltalkScanner scan: ' abc:= ') token
		equals: (ASIdentifierToken with: 'abc').
	self
		assert: (ZagSmalltalkScanner scan: ' abc: ') token
		equals: (ASIdentifierToken with: 'abc:').
	self
		assert: (ZagSmalltalkScanner scan: ' abc:def: ') token class
		equals: ASImproperId.
	self
		assert: (ZagSmalltalkScanner scan: ' abc:def ') token class
		equals: ASImproperId.
	self
		assert: (ZagSmalltalkScanner scan: ' abc ') token arity
		equals: 1.
	self assert: (ZagSmalltalkScanner scan: ' * ') token arity equals: 2.
	self
		assert: (ZagSmalltalkScanner scan: ' abc: ') token arity
		equals: 2
	"self assert: (ASScanner scan: ' abc ') token isSelector.
	self assert: (ASScanner scan: ' * ') token isSelector.
	self assert: (ASScanner scan: ' abc: ') token isSelector.
	self deny: (ASScanner scan: ' abc:def: ') token isSelector.
	self deny: (ASScanner scan: ' abc:def ') token isSelector.
	self deny: (ASScanner scan: ' nil ') token isSelector."
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testPlus1 [

	| scanner |
	scanner := ZagSmalltalkScanner scan:
		           (ASCompileTestClass1 >> #plus1:) sourceCode.
	self assert: scanner peek equals: (ASIdentifierToken with: 'plus1:').
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			(ASIdentifierToken with: 'plus1:').
			(ASIdentifierToken with: 'aNumber').
			(ASSyntax with: '^').
			(ASIdentifierToken with: 'aNumber').
			(ASIdentifierToken with: '+').
			1 asASLiteral }.
	self assert: scanner atEnd
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testSimple [

	self
		assert: (ZagSmalltalkScanner scan: ' ''''''^'''''' ') token
		equals: '''^''' asASString.
	self
		assert: (ZagSmalltalkScanner scan: ' ''^'' ') token
		equals: '^' asASString.
	self
		assert: (ZagSmalltalkScanner scan: ' #''^'' ') token
		equals: (ASSymbol with: #'^').
	self
		assert: (ZagSmalltalkScanner scan: ' #simple ') token
		equals: (ASSymbol with: #simple).
	self
		assert: (ZagSmalltalkScanner scan: ' #self ') token
		equals: (ASSymbol with: #self).
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def: ') token
		equals: (ASSymbol with: #abc:def:).
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def ') token
		equals: (ASSymbol with: #'abc:def').
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def: ') token arity
		equals: 3.
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def ') token arity
		equals: 0.
	self
		assert: (ZagSmalltalkScanner scan: ' ''hello'' ') token
		equals: 'hello' asASString.
	self
		assert: (ZagSmalltalkScanner scan: ' ''can''''t'' ') token
		equals: 'can''t' asASString.
	self
		assert: (ZagSmalltalkScanner scan: ' 42 ') token
		equals: 42 asASLiteral.
	self
		assert: (ZagSmalltalkScanner scan: ' true') token
		equals: (ASSpecial with: #true).
	self
		assert: (ZagSmalltalkScanner scan: ' false') token
		equals: (ASSpecial with: #false).
	self
		assert: (ZagSmalltalkScanner scan: ' nil') token
		equals: (ASSpecial with: #nil).
	self
		assert: (ZagSmalltalkScanner scan: ' #nil') token
		equals: (ASSymbol with: 'nil').
	self
		assert: (ZagSmalltalkScanner scan: ' self') token
		equals: (ASSelfToken with: #self).
	self
		assert: (ZagSmalltalkScanner scan: ' super') token
		equals: (ASSelfToken with: #super).
	self
		assert: (ZagSmalltalkScanner scan: ' thisContext') token
		equals: (ASSpecial with: #thisContext)
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testSyntax [

	self
		assert: (ZagSmalltalkScanner scan: ' ^ ') token
		equals: (ASSyntax with: '^').
	self
		should: [ (ZagSmalltalkScanner scan: ' #+ ') token ]
		raise: Error.
	self should: [ (ZagSmalltalkScanner scan: ' # ') token ] raise: Error.
	self
		assert: (ZagSmalltalkScanner scan: ' #( ') token
		equals: (ASSyntax with: '#(').
	self
		assert: (ZagSmalltalkScanner scan: ' . ') token
		equals: (ASSyntax with: '.').
	self
		assert: (ZagSmalltalkScanner scan: ' : ') token
		equals: (ASSyntax with: ':').
	self
		assert: (ZagSmalltalkScanner scan: ' := ') token
		equals: (ASSyntax with: ':=').
	self
		assert: (ZagSmalltalkScanner scan: ' | ') token
		equals: (ASIdentifierToken with: '|').
	self
		assert: (ZagSmalltalkScanner scan: ' |: ') token
		equals: (ASIdentifierToken with: '|').
	self
		assert: (ZagSmalltalkScanner scan: ' || ') token
		equals: (ASIdentifierToken with: '||').
	self
		assert: (ZagSmalltalkScanner scan: ' |+ ') token
		equals: (ASIdentifierToken with: '|+')
	"self assert: (ASScanner scan: ' ^ ') token isReturn.
	self assert: (ASScanner scan: ' . ') token isSeparator.
	self assert: (ASScanner scan: ' : ') token isColon.
	self assert: $]) isBlockEnd.
	self assert: $}) isArrayEnd.
	self assert: $)) isCloseParen.
	self deny: $[) isBlockEnd.
	self deny: ${) isArrayEnd.
	self deny: $() isCloseParen."
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testWhitespace [

	| sc |
	sc := ZagSmalltalkScanner scan: ' 
	'.
	self assert: sc atEnd.
	self assert: sc separator equals: '
'.
	sc := ZagSmalltalkScanner scan: ' 	
	"this is a comment"
	
	'.
	self assert: sc atEnd.
	self assert: sc separator equals: '
	"this is a comment"

'
]
