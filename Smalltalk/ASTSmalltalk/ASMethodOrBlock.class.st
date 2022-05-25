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

	arguments := aCollection		          collect: [ :symbol | ASArg name: symbol ]
		          as: Array
]

{ #category : #initialization }
ASMethodOrBlock >> initialize [

	super initialize.
	arguments := #(  )
]

{ #category : #scopes }
ASMethodOrBlock >> lookup: symbol [
	self halt.
]

{ #category : #printing }
ASMethodOrBlock >> printOn: s [

	arguments isEmpty ifFalse: [ 
		s << ' arguments: '.
		arguments printOn: s ].
super printOn: s
]
