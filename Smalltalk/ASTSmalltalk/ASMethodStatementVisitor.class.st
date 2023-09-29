Class {
	#name : #ASMethodStatementVisitor,
	#superclass : #ASStatementVisitor,
	#category : #'ASTSmalltalk-Visitors'
}

{ #category : #'as yet unclassified' }
ASMethodStatementVisitor >> emit: anASMethodNode selectors: selectors [ 
	"emit a method"

	| syms methods |
	codeGenerator emitHeader: selectors.
	
	"emit the method"
	self visitMethod: anASMethodNode.
	
	methods := OrderedCollection new.
	methods add: anASMethodNode.
	codeGenerator emitFooter: methods
]

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
ASMethodStatementVisitor >> visitMethod: anASMethodNode [

	| stackVariables stackSize blockVisitor numArgs selector |
	"what are the stack variables?"
	stackVariables := OrderedCollection new.

	"set the selector for each block that needs a closure and add it to the stack variables"
	blockVisitor := ASBlockVisitor selector: anASMethodNode selector.
	blockVisitor visit: anASMethodNode body.

	anASMethodNode buildClosureData: blockVisitor modifiers.
	blockVisitor blocks do: [ :b |
		b closureData: anASMethodNode closureData ].

	stackVariables addAll: blockVisitor closures.
	stackVariables addAll:
		((blockVisitor filterVars: anASMethodNode locals) collect: [ :local |
			 local name ]).

	"capture the size of the stack before arguments are added"
	stackSize := stackVariables size.

	"add the locals, arguments and self"
	stackVariables addAll:
		(anASMethodNode arguments collect: [ :arg | arg name ]) reverse.
	stackVariables addLast: #self.

	codeGenerator pushNamespace: anASMethodNode nameSpace.

	selector := codeGenerator symbolOrVirtual: anASMethodNode selector.


	codeGenerator
		emitMethod: selector
		numLocals: stackSize
		stackSize: anASMethodNode maxBlockLocals
		closureSize: anASMethodNode clousreSize.
	"aStream << 'compileMethod(' << (syms
		 at: anASMethodNode selector
		 ifPresent: [ :value | value ]
		 ifAbsentPut: [ syms virutalName ]) << ',' << stackSize asString
	<< ','
	<< (self maxBlockLocals asString , '+' , self clousreSize asString)
	<< ',.{'.
	aStream cr."

	"verify selector or primitive?"
	anASMethodNode pragmas
		ifEmpty: [ codeGenerator emitVerifySelector ]
		ifNotEmpty: [
			anASMethodNode pragmas do: [ :pragma |
				codeGenerator emitPragma: (pragma arguments at: 1) value ] ].

	codeGenerator emitPushContext.
	"create dummy context"

	blockVisitor
		visitClosureData: codeGenerator
		stackVariables: stackVariables.

	"ouput any block closure"
	blockVisitor blocks doWithIndex: [ :block :index |
		codeGenerator emitPushBlock:
			(block zigClosureString: stackVariables index: index - 1) ].

	"write the instructions"
	anASMethodNode
		writeBody: symbols
		stackVars: stackVariables
		codeGenerator: codeGenerator
		blocks: blockVisitor.

	"write out the implicit return"
	(anASMethodNode body isEmpty or: [
		 anASMethodNode body last isReturn not ]) ifTrue: [
		codeGenerator pushReturn ].
	codeGenerator pushClose.

	"ouput compile methods for the blocks"
	blockVisitor blocks do: [ :block |
		block isNonLocalReturn ifFalse: [
			block write: codeGenerator with: symbols ] ]
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
