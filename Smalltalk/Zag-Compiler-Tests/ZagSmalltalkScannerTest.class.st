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
		equals: { 'simple' asASIdentifier }.
	self
		assert: (ZagSmalltalkScanner scan: ' simple 
	^ 42 ') asArray
		equals: {
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }.
	self
		assert: (ZagSmalltalkScanner scan: ' simple
	^ 42') asArray
		equals: {
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }.
	self
		assert: (ZagSmalltalkScanner scan: '== simple
	^ 42') asArray
		equals: {
				'==' asASIdentifier.
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testAsStream [

	| scanner |
	scanner := ZagSmalltalkScanner scan: ' simple 
	^ 42 '.
	self assert: scanner peek equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			'simple' asASIdentifier.
			$^ asASSyntax.
			42 asASLiteral }.
	self assert: scanner atEnd.
	scanner := ZagSmalltalkScanner scan: ' simple 
	^ 42 '.
	self deny: scanner atEnd.
	self assert: scanner next equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			$^ asASSyntax.
			42 asASLiteral }
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testIDs [

	self
		assert: (ZagSmalltalkScanner scan: ' abc ') token
		equals: 'abc' asASIdentifier.
	self assert: (ZagSmalltalkScanner scan: ' * ') token equals: $* asASIdentifier.
	self
		assert: (ZagSmalltalkScanner scan: ' abc:= ') token
		equals: 'abc' asASIdentifier.
	self
		assert: (ZagSmalltalkScanner scan: ' abc: ') token
		equals: 'abc:' asASIdentifier.
	self
		assert: (ZagSmalltalkScanner scan: ' abc:def: ') token class
		equals: ASImproperId.
	self
		assert: (ZagSmalltalkScanner scan: ' abc:def ') token class
		equals: ASImproperId.
	self assert: (ZagSmalltalkScanner scan: ' abc ') token arity equals: 1.
	self assert: (ZagSmalltalkScanner scan: ' * ') token arity equals: 2.
	self assert: (ZagSmalltalkScanner scan: ' abc: ') token arity equals: 2
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
	self assert: scanner peek equals: 'plus1:' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			'plus1:' asASIdentifier.
			'aNumber' asASIdentifier.
			$^ asASSyntax.
			'aNumber' asASIdentifier.
			'+' asASIdentifier.
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
		equals: #'^' asASSymbol.
	self
		assert: (ZagSmalltalkScanner scan: ' #simple ') token
		equals: #simple asASSymbol.
	self
		assert: (ZagSmalltalkScanner scan: ' #self ') token
		equals: #self asASSymbol.
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def: ') token
		equals: #abc:def: asASSymbol.
	self
		assert: (ZagSmalltalkScanner scan: ' #abc:def ') token
		equals: #'abc:def' asASSymbol.
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
		equals: 'true' asASSpecial.
	self
		assert: (ZagSmalltalkScanner scan: ' false') token
		equals: 'false' asASSpecial.
	self
		assert: (ZagSmalltalkScanner scan: ' nil') token
		equals: 'nil' asASSpecial.
	self
		assert: (ZagSmalltalkScanner scan: ' #nil') token
		equals: 'nil' asASSymbol.
	self
		assert: (ZagSmalltalkScanner scan: ' self') token
		equals: #self asASSelf.
	self
		assert: (ZagSmalltalkScanner scan: ' super') token
		equals: #super asASSelf.
	self
		assert: (ZagSmalltalkScanner scan: ' thisContext') token
		equals: #thisContext asASSpecial
]

{ #category : 'tests' }
ZagSmalltalkScannerTest >> testSyntax [

	self
		assert: (ZagSmalltalkScanner scan: ' ^ ') token
		equals: $^ asASSyntax.
	self should: [ (ZagSmalltalkScanner scan: ' #+ ') token ] raise: Error.
	self should: [ (ZagSmalltalkScanner scan: ' # ') token ] raise: Error.
	self
		assert: (ZagSmalltalkScanner scan: ' #( ') token
		equals: '#(' asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' . ') token
		equals: $. asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' : ') token
		equals: $: asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' := ') token
		equals: ':=' asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' | ') token
		equals: $| asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' |: ') token
		equals: $| asASSyntax.
	self
		assert: (ZagSmalltalkScanner scan: ' || ') token
		equals: '||' asASIdentifier.
	self
		assert: (ZagSmalltalkScanner scan: ' |+ ') token
		equals: '|+' asASIdentifier
	"self assert: (ASScanner scan: ' ^ ') token isReturn.
	self assert: (ASScanner scan: ' . ') token isSeparator.
	self assert: (ASScanner scan: ' : ') token isColon.
	self assert: $] asASSyntax isBlockEnd.
	self assert: $} asASSyntax isArrayEnd.
	self assert: $) asASSyntax isCloseParen.
	self deny: $[ asASSyntax isBlockEnd.
	self deny: ${ asASSyntax isArrayEnd.
	self deny: $( asASSyntax isCloseParen."
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
