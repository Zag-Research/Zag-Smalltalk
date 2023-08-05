Class {
	#name : #ASBlockStatementVisitor,
	#superclass : #ASStatementVisitor,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #adding }
ASBlockStatementVisitor >> addInstruction: instruction withVar: anASVar [
	
	| varName index |
	"check the stack first"
	varName := anASVar name.
	index := self variableIndex: varName.
	index = '-1'
		ifFalse: [ "push the variable from the stack onto the top of the stack"
			instructionStack add: '&e.', instruction, 'Local, ' , index , ', //' , varName ]
		ifTrue: [ "check to see if the variable is located in the block's closure"
			index := closure blockIndex: varName.
			index = '-1'
				ifFalse: [
					| selfIndex |
					selfIndex := self variableIndex: #self.
					instructionStack add:
						'&e.', instruction, 'LocalData, ' , selfIndex , ' + (' , index
						, ' << 8), // ' , varName , ' offset ' , index , ' in local '
						, selfIndex ]
				ifTrue: [ "the variable must be in the block closure"
					| selfIndex closureIndex |
					selfIndex := self variableIndex: #self.
					closureIndex := closure blockIndex: #closureData.
					index := closure closureIndex: anASVar.
					instructionStack add:
						'&e.', instruction, 'LocalDataData, ' , selfIndex , ' + (' , closureIndex
						, ' << 8) + (' , index , ' << 16), // ' , varName , ' offset '
						, index , ' in offset ' , closureIndex , ' in local '
						, selfIndex ] ]
]

{ #category : #'as yet unclassified' }
ASBlockStatementVisitor >> popClosureData: anASVar [

	| instructions |
	"must be in closure data"
	instructions := OrderedCollection new.
	instructions add:
		'&e.popLocalData, ' , (closure variableIndex: #closureData) , ' + ('
		, (closure variableIndex: anASVar) , '<< 8), // ' , anASVar name.
	^ instructions
]

{ #category : #'as yet unclassified' }
ASBlockStatementVisitor >> popValue: anASVar [
self addInstruction: 'pop' withVar: anASVar

]

{ #category : #'as yet unclassified' }
ASBlockStatementVisitor >> visitVariable: anASRef [
	self addInstruction: 'push' withVar: anASRef variable
]
