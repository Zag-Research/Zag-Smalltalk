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
	^ ASRef new variable: aRBVariableNode name
]

{ #category : #visiting }
ASPharoVisitor >> visitLiteralValueNode: aRBLiteralValueNode [ 
	^ ASLiteral new literal: aRBLiteralValueNode value
]

{ #category : #visiting }
ASPharoVisitor >> visitMessageNode: aRBMessageNode [

	^ ASSend new
		  target: (self visit: aRBMessageNode receiver);
		  selector: aRBMessageNode selector
		  args:
			  (aRBMessageNode arguments collect: [ :arg | self visit: arg ])
]

{ #category : #visiting }
ASPharoVisitor >> visitMethodNode: aRBMethodNode [

	| selector method |
	selector := aRBMethodNode selector.
	compiler definingClass: aRBMethodNode methodClass.
	method := ASMethodNode new
		          selector: selector;
		          class: aRBMethodNode methodClass;
		          arguments: (aRBMethodNode arguments collect: #name as: Array);
		          locals:
			          (aRBMethodNode temporaries collect: #name as: Array).
	compiler definingMethod: method.
	compiler inScope: method do: [ 
		aRBMethodNode statements do: [ :statement | 
			method addStatement: (self visit: statement) ] ].
	^ method
]

{ #category : #visiting }
ASPharoVisitor >> visitReturnNode: aRBReturnNode [ 
	^ ASReturn new expression: (self visit: aRBReturnNode value)
]
