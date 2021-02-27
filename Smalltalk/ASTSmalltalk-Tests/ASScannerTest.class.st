Class {
	#name : #ASScannerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

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
	self assert: (ASScanner new scanCode: ' simple') asArray equals: {'simple' asASIdentifier}.
	self assert: (ASScanner new scanCode: ' simple 
	^ 42 ') asArray equals: {'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.
	self assert: (ASScanner new scanCode: ' simple
	^ 42') asArray equals: {'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.
	self assert: (ASScanner new scanCode: '== simple
	^ 42') asArray equals: {'==' asASIdentifier . 'simple' asASIdentifier . $^ asASSyntax . 42 asASLiteral}.

]

{ #category : #tests }
ASScannerTest >> testWhitespace [
	self assert: (ASScanner new scanCode: ' 
	 ') asArray equals: #().

]
