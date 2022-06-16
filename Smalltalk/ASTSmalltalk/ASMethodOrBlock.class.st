"
I contain all the common respersentation for Methods and Blocks
"
Class {
	#name : #ASMethodOrBlock,
	#superclass : #ASCodeBlock,
	#instVars : [
		'arguments'
	],
	#category : #'ASTSmalltalk-AST'
}

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

	arguments := aCollection
		collect: [ :symbol | 
			| arg |
			arg := ASArg name: symbol.
			scope at: symbol put: arg.
			arg ]
		as: Array
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
