Class {
	#name : 'ASScannerTest',
	#superclass : 'TestCase',
	#category : 'Zag-Compiler-Tests-Tests',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASScannerTest >> testAsArray [

	self
		assert: (ASScanner scan: ' simple') asArray
		equals: { 'simple' asASIdentifier }.
	self
		assert: (ASScanner scan: ' simple 
	^ 42 ') asArray
		equals: {
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }.
	self
		assert: (ASScanner scan: ' simple
	^ 42') asArray
		equals: {
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }.
	self
		assert: (ASScanner scan: '== simple
	^ 42') asArray
		equals: {
				'==' asASIdentifier.
				'simple' asASIdentifier.
				$^ asASSyntax.
				42 asASLiteral }
]

{ #category : 'tests' }
ASScannerTest >> testAsStream [

	| scanner |
	scanner := ASScanner scan: ' simple 
	^ 42 '.
	self assert: scanner peek equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			'simple' asASIdentifier.
			$^ asASSyntax.
			42 asASLiteral }.
	self assert: scanner atEnd.
	scanner := ASScanner scan: ' simple 
	^ 42 '.
	self deny: scanner atEnd.
	self assert: scanner next equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {
			$^ asASSyntax.
			42 asASLiteral }
]

{ #category : 'tests' }
ASScannerTest >> testIDs [

	self
		assert: (ASScanner scan: ' abc ') token
		equals: 'abc' asASIdentifier.
	self assert: (ASScanner scan: ' * ') token equals: $* asASIdentifier.
	self
		assert: (ASScanner scan: ' abc:= ') token
		equals: 'abc' asASIdentifier.
	self
		assert: (ASScanner scan: ' abc: ') token
		equals: 'abc:' asASIdentifier.
	self
		assert: (ASScanner scan: ' abc:def: ') token class
		equals: ASImproperId.
	self
		assert: (ASScanner scan: ' abc:def ') token class
		equals: ASImproperId.
	self assert: (ASScanner scan: ' abc ') token arity equals: 1.
	self assert: (ASScanner scan: ' * ') token arity equals: 2.
	self assert: (ASScanner scan: ' abc: ') token arity equals: 2
	"self assert: (ASScanner scan: ' abc ') token isSelector.
	self assert: (ASScanner scan: ' * ') token isSelector.
	self assert: (ASScanner scan: ' abc: ') token isSelector.
	self deny: (ASScanner scan: ' abc:def: ') token isSelector.
	self deny: (ASScanner scan: ' abc:def ') token isSelector.
	self deny: (ASScanner scan: ' nil ') token isSelector."
]

{ #category : 'tests' }
ASScannerTest >> testPlus1 [

	| scanner |
	scanner := ASScanner scan:
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
ASScannerTest >> testSimple [

	self
		assert: (ASScanner scan: ' ''''''^'''''' ') token
		equals: '''^''' asASString.
	self
		assert: (ASScanner scan: ' ''^'' ') token
		equals: '^' asASString.
	self
		assert: (ASScanner scan: ' #''^'' ') token
		equals: #'^' asASSymbol.
	self
		assert: (ASScanner scan: ' #simple ') token
		equals: #simple asASSymbol.
	self
		assert: (ASScanner scan: ' #self ') token
		equals: #self asASSymbol.
	self
		assert: (ASScanner scan: ' #abc:def: ') token
		equals: #abc:def: asASSymbol.
	self
		assert: (ASScanner scan: ' #abc:def ') token
		equals: #'abc:def' asASSymbol.
	self
		assert: (ASScanner scan: ' #abc:def: ') token arity
		equals: 3.
	self
		assert: (ASScanner scan: ' #abc:def ') token arity
		equals: 0.
	self
		assert: (ASScanner scan: ' ''hello'' ') token
		equals: 'hello' asASString.
	self
		assert: (ASScanner scan: ' ''can''''t'' ') token
		equals: 'can''t' asASString.
	self
		assert: (ASScanner scan: ' 42 ') token
		equals: 42 asASLiteral.
	self
		assert: (ASScanner scan: ' true') token
		equals: 'true' asASSpecial.
	self
		assert: (ASScanner scan: ' false') token
		equals: 'false' asASSpecial.
	self
		assert: (ASScanner scan: ' nil') token
		equals: 'nil' asASSpecial.
	self
		assert: (ASScanner scan: ' #nil') token
		equals: 'nil' asASSymbol.
	self
		assert: (ASScanner scan: ' self') token
		equals: #self asASSelf.
	self
		assert: (ASScanner scan: ' super') token
		equals: #super asASSelf.
	self
		assert: (ASScanner scan: ' thisContext') token
		equals: #thisContext asASSpecial
]

{ #category : 'tests' }
ASScannerTest >> testSyntax [

	self
		assert: (ASScanner scan: ' ^ ') token
		equals: $^ asASSyntax.
	self should: [ (ASScanner scan: ' #+ ') token ] raise: Error.
	self should: [ (ASScanner scan: ' # ') token ] raise: Error.
	self
		assert: (ASScanner scan: ' #( ') token
		equals: '#(' asASSyntax.
	self
		assert: (ASScanner scan: ' . ') token
		equals: $. asASSyntax.
	self
		assert: (ASScanner scan: ' : ') token
		equals: $: asASSyntax.
	self
		assert: (ASScanner scan: ' := ') token
		equals: ':=' asASSyntax.
	self
		assert: (ASScanner scan: ' | ') token
		equals: $| asASSyntax.
	self
		assert: (ASScanner scan: ' |: ') token
		equals: $| asASSyntax.
	self
		assert: (ASScanner scan: ' || ') token
		equals: '||' asASIdentifier.
	self
		assert: (ASScanner scan: ' |+ ') token
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
ASScannerTest >> testWhitespace [

	| sc |
	sc := ASScanner scan: ' 
	'.
	self assert: sc atEnd.
	self assert: sc separator equals: '
'.
	sc := ASScanner scan: ' 	
	"this is a comment"
	
	'.
	self assert: sc atEnd.
	self assert: sc separator equals: '
	"this is a comment"

'
]
