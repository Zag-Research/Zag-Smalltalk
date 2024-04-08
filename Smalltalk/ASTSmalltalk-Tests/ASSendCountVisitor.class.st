"
I am a send vistor that counts the number of sends in a method
"
Class {
	#name : 'ASSendCountVisitor',
	#superclass : 'Object',
	#instVars : [
		'count'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'visiting' }
ASSendCountVisitor >> count [ 
^ count
]

{ #category : 'visiting' }
ASSendCountVisitor >> doesNotUnderstand: aMessage [
]

{ #category : 'visiting' }
ASSendCountVisitor >> increment [
	count := count + 1
]

{ #category : 'initialization' }
ASSendCountVisitor >> initialize [

	count := 0
]

{ #category : 'visiting' }
ASSendCountVisitor >> visit: aRBNode [

	aRBNode acceptVisitor: self
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitArgumentVariableNode: aRBVariableNode [

]

{ #category : 'visiting' }
ASSendCountVisitor >> visitArrayNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitAssignmentNode: aRBAssignmentNode [

	self visit: aRBAssignmentNode value.
	self visit: aRBAssignmentNode variable
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitBlockNode: aRBBlockNode [

	aRBBlockNode statements do: [ :s | self visit: s ]
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitCascadeNode: aNode [

	self increment.
	self visit: aNode receiver.
	aNode messages do: [ :m | self visit: m ]
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitClassVariableNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitGlobalVariableNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitInstanceVariableNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitLiteralArrayNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitLiteralValueNode: aNode [
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitLiteralVariableNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitMessageNode: aRBMessageNode [

	self increment.
	self visit: aRBMessageNode receiver.
	aRBMessageNode arguments do: [ :arg | self visit: arg ]
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitMethodNode: aRBMethodNode [

	aRBMethodNode statements do: [ :s | self visit: s ]
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitReturnNode: aRBReturnNode [
	self visit: aRBReturnNode value
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitSelfNode: aRBSelfNode [

]

{ #category : 'visiting' }
ASSendCountVisitor >> visitSend: aRBSendNode [

	self increment
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitSuperNariableNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitSuperNode: aNode [

	
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitTemporaryVariableNode: aNode [

]

{ #category : 'visiting' }
ASSendCountVisitor >> visitThisContextNode: aNode [
]

{ #category : 'visiting' }
ASSendCountVisitor >> visitVariableNode: aNode [

	
]
