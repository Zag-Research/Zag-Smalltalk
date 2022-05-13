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
ASPharoVisitor >> visitMethodNode: aRBMethodNode [

	| selector method |
	selector := aRBMethodNode selector.
	compiler class: aRBMethodNode methodClass.
	compiler definingSelector: selector.
	method := ASMethodNode new
		          selector: selector;
		          arguments: aRBMethodNode arguments;
						locals: (aRBMethodNode temporaries collect: [ : var | self halt.]).
	compiler
		inScope: method
		do: [ aRBMethodNode statements do: [ :statement |  ] ].
	^ method
]
