"
I implement the visitor pattern to walk a Pharo AST and transform it to our AST.
"
Class {
	#name : #ASPharoVisitor,
	#superclass : #Object,
	#instVars : [
		'compiler',
		'variables'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #'instance creation' }
ASPharoVisitor class >> compiler: anASPharoCompiler [
	^ self new compiler: anASPharoCompiler
]

{ #category : #adding }
ASPharoVisitor >> addVariable: variableName [ 
	variables
		at: variableName
		ifPresent: [  ]
		ifAbsentPut: [ OrderedCollection new ].
]

{ #category : #accessing }
ASPharoVisitor >> compiler: anASPharoCompiler [
variables := Dictionary new.
	compiler := anASPharoCompiler
]

{ #category : #visiting }
ASPharoVisitor >> visit: aRBNode [
	^ aRBNode acceptVisitor: self	
]

{ #category : #visiting }
ASPharoVisitor >> visitArgumentVariableNode: aRBVariableNode [
	self addVariable: aRBVariableNode name.
	^ ASRef variable: (ASArg new name: aRBVariableNode name)
]

{ #category : #visiting }
ASPharoVisitor >> visitArrayNode: aRBArrayNode [

	^ ASArray statements:
		  (aRBArrayNode statements collect: [ :statement | 
			   self visit: statement ] as: Array)
]

{ #category : #visiting }
ASPharoVisitor >> visitAssignmentNode: aRBAssignmentNode [
	^ ASAssign
		  variable: (compiler lookup: aRBAssignmentNode variable name)
		  expression: (self visit: aRBAssignmentNode value)
]

{ #category : #visiting }
ASPharoVisitor >> visitBlockNode: aRBBlockNode [

	^ ASBlock
		  arguments: aRBBlockNode argumentNames
		  locals: aRBBlockNode temporaryNames
		  body: ASSequence new.
"	compiler inScope: block do: [
		aRBBlockNode statements do: [ :statement |
			block addStatement: (self visit: statement) ] ].
	^ block"
]

{ #category : #visiting }
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

{ #category : #visiting }
ASPharoVisitor >> visitClassVariableNode: aRBVariableNode [
self addVariable: aRBVariableNode name.
	^ ASRef variable: (ASClassVar name: aRBVariableNode name)
]

{ #category : #visiting }
ASPharoVisitor >> visitGlobalVariableNode: aRBVariableNode [
self addVariable: aRBVariableNode name.
	^ ASRef variable: (ASGlobal name: aRBVariableNode name)
]

{ #category : #visiting }
ASPharoVisitor >> visitInstanceVariableNode: aRBVariableNode [
self addVariable: aRBVariableNode name.
	^ ASRef variable: (ASInstance name: aRBVariableNode name)
]

{ #category : #visiting }
ASPharoVisitor >> visitLiteralArrayNode: aRBLiteralArrayNode [ 
	^ ASLiteral literal: (aRBLiteralArrayNode contents collect: #value as: Array)
]

{ #category : #visiting }
ASPharoVisitor >> visitLiteralValueNode: aRBLiteralValueNode [

	^ ASLiteral literal: aRBLiteralValueNode value
]

{ #category : #visiting }
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

{ #category : #visiting }
ASPharoVisitor >> visitMethodNode: aRBMethodNode [

	| selector method |
	selector := aRBMethodNode selector.
	compiler definingClass: aRBMethodNode methodClass.
	method := ASMethodNode
		          class: aRBMethodNode methodClass
		          selector: selector
		          arguments:
		          (aRBMethodNode arguments collect: #name as: Array)
		          locals:
		          (aRBMethodNode temporaries collect: #name as: Array)
		          body: ASSequence new.
	method pragmas: (aRBMethodNode pragmas do: [ :pram | self visit: pram  ]).
	compiler definingMethod: method.
	compiler inScope: method do: [ 
		aRBMethodNode statements do: [ :statement | 
			method addStatement: (self visit: statement) ] ].
	^ method
]

{ #category : #visiting }
ASPharoVisitor >> visitPragmaNode: aRBPragmaNode [

	^ ASPragma args:
		  (aRBPragmaNode arguments collect: [ :arg | self visit: arg ]) 
]

{ #category : #visiting }
ASPharoVisitor >> visitReturnNode: aRBReturnNode [

	^ ASReturn expression: (self visit: aRBReturnNode value)
]

{ #category : #visiting }
ASPharoVisitor >> visitSelfNode: aRBVariableNode [

	^ ASSelf method: compiler definingMethod.

]

{ #category : #visiting }
ASPharoVisitor >> visitSuperNode: aRBVariableNode [
		^ ASSuper method: compiler definingMethod superclass: compiler definingClass superclass 
]

{ #category : #visiting }
ASPharoVisitor >> visitTemporaryVariableNode: aRBVariableNode [
	self addVariable: aRBVariableNode name.
	^ ASRef variable: (ASLocal name: aRBVariableNode name)
]

{ #category : #visiting }
ASPharoVisitor >> visitThisContextNode: anRBVariableNode [
	^ ASInstance name: anRBVariableNode name
]
