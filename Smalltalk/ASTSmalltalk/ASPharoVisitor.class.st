"
I implement the visitor pattern to walk a Pharo AST and transform it to our AST.
"
Class {
	#name : #ASPharoVisitor,
	#superclass : #Object,
	#instVars : [
		'compiler'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #'instance creation' }
ASPharoVisitor class >> compiler: anASPharoCompiler [
	^ self new compiler: anASPharoCompiler
]

{ #category : #accessing }
ASPharoVisitor >> compiler: anASPharoCompiler [
	compiler := anASPharoCompiler
]

{ #category : #visiting }
ASPharoVisitor >> visit: aRBNode [
	^ aRBNode acceptVisitor: self	
]

{ #category : #visiting }
ASPharoVisitor >> visitArgumentVariableNode: aRBVariableNode [

	^ ASArg new name: aRBVariableNode name
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
		  body: (aRBBlockNode statements collect: [ :statement | 
				   self visit: statement ] as: Array)
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

	^ ASClassVar name: aRBVariableNode name
]

{ #category : #visiting }
ASPharoVisitor >> visitGlobalVariableNode: aRBVariableNode [

	^ ASGlobal name: aRBVariableNode name
]

{ #category : #visiting }
ASPharoVisitor >> visitInstanceVariableNode: aRBVariableNode [

	^ ASInstance name: aRBVariableNode name
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
	target tell: compiler youAreSent: selector.
	^ ASSend
		  target: target
		  selector: selector args: (aRBMessageNode arguments
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
		          body: OrderedCollection new.
	compiler definingMethod: method.
	compiler inScope: method do: [ 
		aRBMethodNode statements do: [ :statement | 
			method addStatement: (self visit: statement) ] ].
	^ method
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

		^ ASSelf method: compiler definingMethod superclass: compiler definingClass superclass 
]

{ #category : #visiting }
ASPharoVisitor >> visitTemporaryVariableNode: aRBVariableNode [

	^ASLocal name: aRBVariableNode name
]