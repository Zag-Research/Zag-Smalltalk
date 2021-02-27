Class {
	#name : #ASScannerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASScannerTest >> testSimple [
	self assert: (ASScanner new scanCode: ' simple
	^ 42 ') asArray equals: {'simple' asASIdentifier . $^ . 42}.

]

{ #category : #tests }
ASScannerTest >> testWhitespace [
	self assert: (ASScanner new scanCode: ' 
	 ') asArray equals: #().

]
