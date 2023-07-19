Class {
	#name : #ASStatementVisitor,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'stackVars',
		'aStream'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #instantiating }
ASStatementVisitor class >> symbols: syms stackVars: stackVars with: aStream [

	
	^ self new symbols: syms stackVars: stackVars with: aStream
]

{ #category : #constructor }
ASStatementVisitor >> symbols: syms stackVars: stackVariables with: aStreamWriter [

	symbols := syms.
	stackVars := stackVariables.
	aStream := aStreamWriter
]

{ #category : #visiting }
ASStatementVisitor >> visitAssign: anASAssign [

	anASAssign expression acceptVisitor: self.
	aStream cr.
	aStream << '&e.popLocal, '.
	anASAssign variable acceptVisitor: self.
	aStream << ','
]

{ #category : #visiting }
ASStatementVisitor >> visitBlockNode: anASBlock [ 
| nonLocalReturnValue |
nonLocalReturnValue := anASBlock nonLocalReturn: stackVars.
	nonLocalReturnValue ifNotNil: [
		aStream << nonLocalReturnValue ]
	ifNil: [ 
	aStream << '&e.pushLocal,' << (stackVars indexOf: anASBlock selector) asString << ',' ].
	aStream cr.
]

{ #category : #visiting }
ASStatementVisitor >> visitLiteral: anASLiteral [

	aStream << ' &e.pushLiteral, Object.from('
	<< anASLiteral literal asString << '),'.
	aStream cr
]

{ #category : #visiting }
ASStatementVisitor >> visitNamedAS: anASLocal [ 
	aStream << ((stackVars indexOf: anASLocal name) - 1) asString
]

{ #category : #visiting }
ASStatementVisitor >> visitReturn: anASReturn [ 
	anASReturn expression acceptVisitor: self.
	aStream << ' &e.returnTop,'.
	aStream cr
]

{ #category : #visiting }
ASStatementVisitor >> visitSelfNode: anASSelf [

	aStream << '&e.pushLocal, '
	<< ((stackVars indexOf: 'self') - 1) asString<< ','.
	aStream cr
]

{ #category : #visiting }
ASStatementVisitor >> visitSend: anASSend [

	anASSend target acceptVisitor: self.
	anASSend args ifNotNil: [
		anASSend args do: [ :arg | arg acceptVisitor: self ] ].
	aStream << '&e.send' << anASSend selector numArgs asString << ', '.
	symbols
		at: anASSend selector
		ifPresent: [ :sym | aStream << sym ]
		ifAbsent: [
			self halt.
			aStream << anASSend selector ].
	aStream << ','.
	aStream cr
]

{ #category : #visiting }
ASStatementVisitor >> visitVariable: anASRef [ 
	aStream << '&e.pushLocal, ' << (stackVars indexOf: anASRef variable name) asString
	 << ','.
	aStream cr.
]
