Class {
	#name : #ASStatementVisitor,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'stackVars',
		'aStream',
		'instructionStack'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #instantiating }
ASStatementVisitor class >> symbols: syms stackVars: stackVars [

	^ self new symbols: syms stackVars: stackVars
]

{ #category : #instantiating }
ASStatementVisitor class >> symbols: syms stackVars: stackVars with: aStream [

	
	^ self new symbols: syms stackVars: stackVars with: aStream
]

{ #category : #adding }
ASStatementVisitor >> addDrop [
	instructionStack add:  '&e.drop,'
]

{ #category : #accessing }
ASStatementVisitor >> instructionStack [
	^ instructionStack
]

{ #category : #constructor }
ASStatementVisitor >> symbols: syms stackVars: stackVariables [

	symbols := syms.
	stackVars := stackVariables.
	instructionStack := OrderedCollection new.
	
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
	instructionStack add: '&e.popLocal, ', ((stackVars indexOf: anASAssign variable name) - 1) asString, ','.
	
]

{ #category : #visiting }
ASStatementVisitor >> visitBlockNode: anASBlock [

	| nonLocalReturnValue |
	nonLocalReturnValue := anASBlock nonLocalReturn: stackVars.
	nonLocalReturnValue
		ifNotNil: [
			instructionStack add: nonLocalReturnValue ]
		ifNil: [
			instructionStack add: '&e.pushLocal, ', (stackVars indexOf: anASBlock selector) asString, ',' ].
]

{ #category : #visiting }
ASStatementVisitor >> visitLiteral: anASLiteral [

	instructionStack add: ' &e.pushLiteral, Object.from(', anASLiteral literal asString, '),'.
]

{ #category : #visiting }
ASStatementVisitor >> visitNamedAS: anASLocal [ 
	aStream << ((stackVars indexOf: anASLocal name) - 1) asString
]

{ #category : #visiting }
ASStatementVisitor >> visitReturn: anASReturn [
	anASReturn expression acceptVisitor: self.
	instructionStack add: ' &e.returnTop,'
]

{ #category : #visiting }
ASStatementVisitor >> visitSelfNode: anASSelf [

	instructionStack add: '&e.pushLocal, ', ((stackVars indexOf: 'self') - 1) asString, ','
]

{ #category : #visiting }
ASStatementVisitor >> visitSend: anASSend [

	| send |
	anASSend target acceptVisitor: self.
	anASSend args ifNotNil: [
		anASSend args do: [ :arg | arg acceptVisitor: self ] ].
	send := '&e.send' , anASSend selector numArgs asString , ', '.
	symbols
		at: anASSend selector
		ifPresent: [ :sym | send := send, sym ]
		ifAbsent: [
			self halt.
			send := send, anASSend selector ].
	instructionStack add: send , ','
]

{ #category : #visiting }
ASStatementVisitor >> visitVariable: anASRef [

	instructionStack add: '&e.pushLocal, ', (stackVars indexOf: anASRef variable name) asString, ','
]
