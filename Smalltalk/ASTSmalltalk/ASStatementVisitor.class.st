Class {
	#name : #ASStatementVisitor,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'stackVars',
		'instructionStack',
		'closure',
		'variableTypeMap',
		'codeGenerator'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #adding }
ASStatementVisitor class >> codeGenerator: anASZigGenerator [ 
	^ self new codeGenerator: anASZigGenerator 
]

{ #category : #instantiating }
ASStatementVisitor class >> symbols: syms stackVars: stackVars closure: aClosure [

	^ self new symbols: syms stackVars: stackVars closure: aClosure
]

{ #category : #instantiating }
ASStatementVisitor class >> symbols: syms stackVars: stackVars closure: aClosure variableTypes: variableTypeMap [

	^ self new symbols: syms stackVars: stackVars closure: aClosure
]

{ #category : #adding }
ASStatementVisitor >> codeGenerator: aCodeGenerator [
	codeGenerator := aCodeGenerator 
]

{ #category : #accessing }
ASStatementVisitor >> findClosures: anASVar [
	self shouldBeImplemented.
]

{ #category : #accessing }
ASStatementVisitor >> instructionStack [
	^ instructionStack
]

{ #category : #visiting }
ASStatementVisitor >> popValue: anASVar [

	self subclassResponsibility 
]

{ #category : #constructor }
ASStatementVisitor >> symbols: syms stackVars: stackVariables closure: aClosure [

	symbols := syms.
	stackVars := stackVariables.
	closure := aClosure.
	instructionStack := OrderedCollection new.
	variableTypeMap := Dictionary new.
]

{ #category : #constructor }
ASStatementVisitor >> symbols: syms stackVars: stackVariables closure: aClosure instructionStack: instrStack variablesTypes: variableTypes [

	symbols := syms.
	stackVars := stackVariables.
	closure := aClosure.
	instructionStack := OrderedCollection new.
	variableTypeMap := variableTypes
]

{ #category : #'as yet unclassified' }
ASStatementVisitor >> unifySelector: aSelector withTarget: anASRef [

	| varName methodClasses |
	anASRef isASRef ifFalse: [ ^ self ].

	varName := anASRef variable name.
	methodClasses :=  aSelector implementors collect: [ :impl | impl methodClass  ].
	variableTypeMap
		at: varName
		ifPresent: [ :implementors |
			variableTypeMap
				at: varName
				put: (implementors intersection: methodClasses)]
		ifAbsentPut: [ methodClasses ]
]

{ #category : #visiting }
ASStatementVisitor >> variableIndex: varName [
	^((stackVars indexOf: varName) - 1) asString
]

{ #category : #visiting }
ASStatementVisitor >> visitAssign: anASAssign [

	anASAssign expression acceptVisitor: self.
	self popValue: anASAssign variable.
]

{ #category : #visiting }
ASStatementVisitor >> visitBlockNode: anASBlock [

	| nonLocalReturnValue |
	nonLocalReturnValue := anASBlock nonLocalReturn: stackVars.
	nonLocalReturnValue
		ifNotNil: [ instructionStack add: nonLocalReturnValue ]
		ifNil: [
			codeGenerator pushLocal: anASBlock selector index: (self variableIndex: anASBlock selector asZigString)
			"instructionStack add: '&e.pushLocal, '
				, (self variableIndex: anASBlock selector asZigString) , ', //'
				, anASBlock selector asZigString" ]
]

{ #category : #visiting }
ASStatementVisitor >> visitLiteral: anASLiteral [

	codeGenerator pushLiteral: anASLiteral literal
]

{ #category : #visiting }
ASStatementVisitor >> visitReturn: anASReturn [

	anASReturn expression acceptVisitor: self.
	codeGenerator pushReturn
]

{ #category : #visiting }
ASStatementVisitor >> visitSelfNode: anASSelf [

	codeGenerator pushLocal: #self index: (self variableIndex: #self)
	"instructionStack add:
		'&e.pushLocal, ' , (self variableIndex: #self) , ', //self'"
]

{ #category : #visiting }
ASStatementVisitor >> visitSend: anASSend [

	| selector |
	anASSend target acceptVisitor: self.

	anASSend args ifNotNil: [
		anASSend args do: [ :arg | arg acceptVisitor: self ] ].

	selector := codeGenerator symbol: anASSend selector.

	codeGenerator
		pushSendForSelector: selector
		numArgs: anASSend selector numArgs.

	"send := '&e.send' , anASSend selector numArgs asString , ', '.

	""looking at the classes that implement a particular send""
	self unifySelector: anASSend selector withTarget: anASSend target.

	""looking up the selector from the predefined symbols list or use the selector name itself""
	symbols
		at: anASSend selector
		ifPresent: [ :sym | instructionStack add: send , sym , ',' ]
		ifAbsent: [ instructionStack add: send , anASSend selector , ',' ]"
]
