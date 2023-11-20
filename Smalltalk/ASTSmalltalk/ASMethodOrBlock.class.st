"
I contain all the common respersentation for Methods and Blocks
"
Class {
	#name : #ASMethodOrBlock,
	#superclass : #ASCodeBlock,
	#instVars : [
		'arguments'
	],
	#category : 'ASTSmalltalk-AST'
}

{ #category : #'tests - instance creation' }
ASMethodOrBlock class >> arguments: args locals: locals body: body [

	^ (self locals: locals
		  body: body)
		  arguments: args
		  
]

{ #category : #comparing }
ASMethodOrBlock >> = other [

	^ super = other and: [ 
		  self arguments = other arguments  ]
]

{ #category : #accessing }
ASMethodOrBlock >> arguments [
	^ arguments
]

{ #category : #accessing }
ASMethodOrBlock >> arguments: aCollection [

	arguments := aCollection asArray
]

{ #category : #accessing }
ASMethodOrBlock >> children [
	| c |
	c := super children.
	c addAll: arguments.
	^ c
]

{ #category : #printing }
ASMethodOrBlock >> compileMethod: aCompiledMethod [ 
| method |
	method := (ASPharoVisitor compiler: self) visit: aCompiledMethod ast.
	method instructions:  (aCompiledMethod ir accept: (ASPharoVisitor compiler: self)).
	^ method
]

{ #category : #initialization }
ASMethodOrBlock >> initialize [

	super initialize.
	arguments := #(  )
]

{ #category : #printing }
ASMethodOrBlock >> printOn: s [

	arguments isEmpty ifFalse: [ 
		s << ' arguments: #('.
		arguments do: [ :arg| s<< arg name] separatedBy: [ s<<' ' ].
		s<< ')' ].
	super printOn: s
]
