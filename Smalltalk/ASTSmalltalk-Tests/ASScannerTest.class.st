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
ASScannerTest >> testWhitespace [
	self assert: (ASScanner new scanCode: ' 
	 ') atEnd.

]
