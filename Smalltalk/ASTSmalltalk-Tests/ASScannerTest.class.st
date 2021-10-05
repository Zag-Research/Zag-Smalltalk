Class {
	#name : #ASScannerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASScannerTest >> testAsArray [
	self assert: (ASScanner new scanCode: ' simple') asArray equals: {'simple' asASIdentifier}.
	self assert: (ASScanner new scanCode: ' simple 
	^ 42 ') asArray equals: {'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.
	self assert: (ASScanner new scanCode: ' simple
	^ 42') asArray equals: {'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.
	self assert: (ASScanner new scanCode: '== simple
	^ 42') asArray equals: {'==' asASIdentifier . 'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.

]

{ #category : #tests }
ASScannerTest >> testAsStream [
	| scanner |
	scanner := ASScanner new scanCode: ' simple 
	^ 42 '.
	self assert: scanner peek equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.
	self assert: scanner atEnd.
	scanner := ASScanner new scanCode: ' simple 
	^ 42 '.
	self deny: scanner atEnd.
	self assert: scanner next equals: 'simple' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {$^ asASSyntax . 42 asASLiteral}.

]

{ #category : #tests }
ASScannerTest >> testIDs [
	self assert: (ASScanner new scanCode: ' abc ') token equals:  'abc' asASIdentifier.
	self assert: (ASScanner new scanCode: ' * ') token equals:  '*' asASIdentifier.
	self assert: (ASScanner new scanCode: ' abc:= ') token equals:  'abc' asASIdentifier.
	self assert: (ASScanner new scanCode: ' abc: ') token equals:  'abc:' asASIdentifier.
	self assert: (ASScanner new scanCode: ' abc:def: ') token equals:  'abc:def:' asASIdentifier.
	self assert: (ASScanner new scanCode: ' abc:def ') token equals:  'abc:def' asASIdentifier.
	self assert: (ASScanner new scanCode: ' abc ') token arity equals:  1.
	self assert: (ASScanner new scanCode: ' * ') token arity equals:  2.
	self assert: (ASScanner new scanCode: ' abc: ') token arity equals:  2.
	self assert: (ASScanner new scanCode: ' abc:def: ') token arity equals:  3.
	self assert: (ASScanner new scanCode: ' abc:def ') token arity equals:  0.
	self assert: (ASScanner new scanCode: ' abc ') token isMethodIntro.
	self assert: (ASScanner new scanCode: ' * ') token isMethodIntro.
	self assert: (ASScanner new scanCode: ' abc: ') token isMethodIntro.
	self deny: (ASScanner new scanCode: ' abc:def: ') token isMethodIntro.
	self deny: (ASScanner new scanCode: ' abc:def ') token isMethodIntro.
	self deny: (ASScanner new scanCode: ' nil ') token isMethodIntro.

]

{ #category : #tests }
ASScannerTest >> testPlus1 [
	| scanner |
	scanner := ASScanner new scanCode: (ASCompileTestClass1>>#plus1:) sourceCode.
	self assert: scanner peek equals: 'plus1:' asASIdentifier.
	self deny: scanner atEnd.
	self assert: scanner asArray equals: {'plus1:' asASIdentifier . 'aNumber' asASIdentifier . $^ asASSyntax . 'aNumber' asASIdentifier . '+' asASIdentifier . 1 asASLiteral }.
	self assert: scanner atEnd.

]

{ #category : #tests }
ASScannerTest >> testSimple [
	self assert: (ASScanner new scanCode: ' ''''''^'''''' ') token equals: '''^''' asASString.
	self assert: (ASScanner new scanCode: ' ''^'' ') token equals: '^' asASString.
	self assert: (ASScanner new scanCode: ' #''^'' ') token equals: #'^' asASSymbol.
	self assert: (ASScanner new scanCode: ' #simple ') token equals: #simple asASSymbol.
	self assert: (ASScanner new scanCode: ' ''hello'' ') token equals: 'hello' asASString.
	self assert: (ASScanner new scanCode: ' ''can''''t'' ') token equals: 'can''t' asASString.
	self assert: (ASScanner new scanCode: ' 42 ') token equals:  42 asASLiteral.
	self assert: (ASScanner new scanCode: ' true') token equals: 'true' asASSpecial.
	self assert: (ASScanner new scanCode: ' false') token equals: 'false' asASSpecial.
	self assert: (ASScanner new scanCode: ' nil') token equals: 'nil' asASSpecial.
	self assert: (ASScanner new scanCode: ' self') token equals: #self asASSelf.
	self assert: (ASScanner new scanCode: ' super') token equals: #super asASSelf.
	self assert: (ASScanner new scanCode: ' thisContext') token equals: #thisContext asASSpecial.

]

{ #category : #tests }
ASScannerTest >> testSyntax [
	self assert: (ASScanner new scanCode: ' ^ ') token equals:  $^ asASSyntax.
	self assert: (ASScanner new scanCode: ' . ') token equals:  $. asASSyntax.
	self assert: (ASScanner new scanCode: ' : ') token equals:  $: asASSyntax.
	self assert: (ASScanner new scanCode: ' := ') token equals:  ':=' asASSyntax.
	self assert: (ASScanner new scanCode: ' ^ ') token isReturn.
	self assert: (ASScanner new scanCode: ' . ') token isSeparator.
	self assert: (ASScanner new scanCode: ' : ') token isColon.
	self assert: (ASScanner new scanCode: ' := ') token isAssignment.
	self assert: (ASScanner new scanCode: ' :> ') token isParrot.
	self assert: (ASScanner new scanCode: ' [ ') token isOpenBracket.
	self assert: (ASScanner new scanCode: ' { ') token isOpenBracket.
	self assert: (ASScanner new scanCode: ' ( ') token isOpenBracket.
	self assert: (ASScanner new scanCode: ' [ ') token isBlockStart.
	self assert: (ASScanner new scanCode: ' { ') token isArrayBuild.
	self assert: (ASScanner new scanCode: ' ( ') token isParen.
	self assert: ($] asASSyntax matchesBracket: $[ asASSyntax).
	self assert: ($} asASSyntax matchesBracket: ${ asASSyntax).
	self assert: ($) asASSyntax matchesBracket: $( asASSyntax).
	self assert: ($] asASSyntax matchesBracket: $[).
	self assert: ($} asASSyntax matchesBracket: ${).
	self assert: ($) asASSyntax matchesBracket: $().

]

{ #category : #tests }
ASScannerTest >> testWhitespace [
	self assert: (ASScanner new scanCode: ' 
	 ') atEnd.

]
