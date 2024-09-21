"
I implement the visitor pattern to walk a Pharo AST and transform it to our AST.
"
Class {
	#name : 'ASPharoVisitor',
	#superclass : 'Object',
	#instVars : [
		'compiler',
		'variables'
	],
	#category : 'ASTSmalltalk-Compiler',
	#package : 'ASTSmalltalk',
	#tag : 'Compiler'
}

{ #category : 'instance creation' }
ASPharoVisitor class >> compiler: anASPharoCompiler [
	^ self new compiler: anASPharoCompiler
]

{ #category : 'adding' }
ASPharoVisitor >> addVariable: variableName [ 
	variables
		at: variableName
		ifPresent: [  ]
		ifAbsentPut: [ OrderedCollection new ].
]

{ #category : 'accessing' }
ASPharoVisitor >> compiler: anASPharoCompiler [
variables := Dictionary new.
	compiler := anASPharoCompiler
]

{ #category : 'visiting' }
ASPharoVisitor >> visit: aRBNode [
	^ aRBNode acceptVisitor: self	
]

{ #category : 'visiting' }
ASPharoVisitor >> visitArgumentVariableNode: aRBVariableNode [

	^ ASRef variable: aRBVariableNode name
]

{ #category : 'visiting' }
ASPharoVisitor >> visitArrayNode: aRBArrayNode [

	^ (aRBArrayNode statements
			   collect: [ :statement | self visit: statement ]
			   as: ASArray)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitAssignmentNode: aRBAssignmentNode [

	^ ASAssign
		  variable: (aRBAssignmentNode variable name)
		  expression: (self visit: aRBAssignmentNode value)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitBlockNode: aRBBlockNode [

	| block body |
	body := ASSequence new: aRBBlockNode statements size.
	block := ASBlock
		  arguments: aRBBlockNode argumentNames
		  locals: aRBBlockNode temporaryNames
		  body: body.
	compiler inScope: block do: [
		aRBBlockNode statements doWithIndex: [ :statement :index |
			body at: index put: (self visit: statement) ] ].
	^ block
]

{ #category : 'visiting' }
ASPharoVisitor >> visitCascadeNode: aRBCascadeNode [

	^ ASCascade
		  target: (self visit: aRBCascadeNode receiver)
		  sends: (aRBCascadeNode messages
				   collect: [ :message | 
					   ASSend
						   selector: message selector
						   args: (message arguments collect: [: arg| self visit: arg  ] as: Array) ]
				   as: Array)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitClassVariableNode: aRBVariableNode [

	^ ASRef variable: aRBVariableNode name 
]

{ #category : 'visiting' }
ASPharoVisitor >> visitGlobalVariableNode: aRBVariableNode [

	^ ASRef variable: aRBVariableNode name
]

{ #category : 'visiting' }
ASPharoVisitor >> visitInstanceVariableNode: aRBVariableNode [

	^ ASRef variable: aRBVariableNode name
]

{ #category : 'visiting' }
ASPharoVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	^ ASLiteral literal: (aRBLiteralArrayNode contents collect: #value as: Array)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitLiteralValueNode: aRBLiteralValueNode [

	^ ASLiteral literal: aRBLiteralValueNode value
]

{ #category : 'visiting' }
ASPharoVisitor >> visitMessageNode: aRBMessageNode [

	| target selector |
	target := self visit: aRBMessageNode receiver.
	selector := aRBMessageNode selector.
	compiler visitMessages ifTrue: [
		target tell: compiler youAreSent: selector ].
	^ ASSend
		  target: target
		  selector: selector
		  args: (aRBMessageNode arguments
				   collect: [ :arg | self visit: arg ]
				   as: Array)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitMethodNode: aRBMethodNode [

	| selector method |
	selector := aRBMethodNode selector.
	compiler definingClass: aRBMethodNode methodClass.
	method := ASMethodNode
		          selector: selector
 pragmas:
		(aRBMethodNode pragmas collect: [ :pram | self visit: pram ])
				          arguments: aRBMethodNode argumentNames
		          locals: aRBMethodNode temporaryNames
		          body: (ASSequence new: aRBMethodNode statements size).
	compiler definingMethod: method.
	compiler inScope: method do: [
		aRBMethodNode statements doWithIndex: [ :statement :index |
			method body at: index put: (self visit: statement) ] ].
	^ method
]

{ #category : 'visiting' }
ASPharoVisitor >> visitPragmaNode: aRBPragmaNode [

	^ ASPragma args:
		  (aRBPragmaNode arguments collect: [ :arg | self visit: arg ]) 
]

{ #category : 'visiting' }
ASPharoVisitor >> visitReturnNode: aRBReturnNode [

	^ ASReturn expression: (self visit: aRBReturnNode value)
]

{ #category : 'visiting' }
ASPharoVisitor >> visitSelfNode: aRBVariableNode [

	^ ASSelf new
]

{ #category : 'visiting' }
ASPharoVisitor >> visitSuperNode: aRBVariableNode [

	^ ASSuper new
]

{ #category : 'visiting' }
ASPharoVisitor >> visitTemporaryVariableNode: aRBVariableNode [

	^ ASRef variable: aRBVariableNode name
]

{ #category : 'visiting' }
ASPharoVisitor >> visitThisContextNode: anRBVariableNode [

	^ ASThisContext new
]
