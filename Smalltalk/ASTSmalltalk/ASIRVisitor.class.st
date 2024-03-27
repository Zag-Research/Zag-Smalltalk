Class {
	#name : #ASIRVisitor,
	#superclass : #Object,
	#instVars : [
		'compiler'
	],
	#category : 'ASTSmalltalk-Image'
}

{ #category : #accessing }
ASIRVisitor class >> compiler: anASPharoCompiler [
	^ self new compiler: anASPharoCompiler
]

{ #category : #compiling }
ASIRVisitor >> compileMethod: aCompiledMethod [ 
| method |
	method := (ASPharoVisitor compiler: self) visit: aCompiledMethod ast.
	method instructions:  (aCompiledMethod ir accept: (ASIRVisitor compiler: self)).
	^ method
]

{ #category : #compiling }
ASIRVisitor >> compiler: anASPharoCompiler [ 
	compiler := anASPharoCompiler
]

{ #category : #compiling }
ASIRVisitor >> visitPushReceiver: anIRPushReceiver [ 
	self shouldBeImplemented.
]
