"
I create an array from a sequence of expressions { axp1 . exp2 }
"
Class {
	#name : 'ASArray',
	#superclass : 'ASExpression',
	#type : 'variable',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASArray class >> statements: aCollection [

	^ aCollection collect: #yourself as: self
]

{ #category : 'comparing' }
ASArray >> = other [

	^ self class = other class and: [ 
			  self statements = other statements ]
]

{ #category : 'printing' }
ASArray >> do: aBlock [
	1 to: self size do: [ : idx| aBlock value: (self at: idx) ]
]

{ #category : 'private' }
ASArray >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.
	Collect the resulting values into self. Answer self."

	| index |
	index := 0.
	aCollection do: [ :each |
		self at: (index := index + 1) put: (aBlock value: each) ]
]

{ #category : 'printing' }
ASArray >> printOn: s [

	s << self class name << ' {'.
	self
		do: [ :statement | statement printOn: s ]
		separatedBy: [ s << ' . ' ].
	s << '}'
]

{ #category : 'compiling' }
ASArray >> zigWalk: aGenerator [

	self do: [ : each| each zigWalk: aGenerator ].
	^ aGenerator pushArray: self size
]
