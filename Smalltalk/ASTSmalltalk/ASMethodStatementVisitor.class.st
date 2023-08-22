Class {
	#name : #ASMethodStatementVisitor,
	#superclass : #ASStatementVisitor,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'as yet unclassified' }
ASMethodStatementVisitor >> popClosureData: anASVar [

	| instructions |
	"must be in closure data"
	instructions := OrderedCollection new.
	instructions add:
		'&e.popLocalData, ' , (self variableIndex: #closureData) , ' + ('
		, (closure variableIndex: anASVar) , '<< 12), // ' , anASVar name.
	^ instructions
]

{ #category : #'as yet unclassified' }
ASMethodStatementVisitor >> popValue: anASVar [

		(stackVars includes: anASVar name)
		ifTrue: [ "a simple case"
			instructionStack add:
				'&e.popLocal, ' , (self variableIndex: anASVar name)
				, ', //' , anASVar name ]
		ifFalse: [ "the variable is in a closure"
			instructionStack addAll: (self popClosureData: anASVar) ]
]

{ #category : #'as yet unclassified' }
ASMethodStatementVisitor >> pushLocalData: anASVar [

	^ '&e.pushLocalData, ' , (self variableIndex: #closureData) , ' + ('
	  , (closure variableIndex: anASVar) , ' << 12), // l1'
]

{ #category : #visiting }
ASMethodStatementVisitor >> visitVariable: anASRef [

	| index |
	index := self variableIndex: anASRef variable name.
	index = '-1'
		ifTrue: [
			instructionStack add: (self pushLocalData: anASRef variable) ]
		ifFalse: [
			instructionStack add:
				'&e.pushLocal, ' , index , ', //' , anASRef variable name ]
]
