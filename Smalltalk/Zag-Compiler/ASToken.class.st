"
I am the tokens for the AST scanner
"
Class {
	#name : 'ASToken',
	#superclass : 'Object',
	#instVars : [
		'value'
	],
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'comparing' }
ASToken >> = other [
	^ self class = other class and: [value = other value]
]

{ #category : 'testing' }
ASToken >> atEnd [
	^ false
]

{ #category : 'parsing' }
ASToken >> classSelector: anASCompiler args: aCollection selector: selector [ 
	^ selector asSymbol
]

{ #category : 'parsing' }
ASToken >> defs: anASCompiler scope: anASMethodNode [ 

]

{ #category : 'parsing' }
ASToken >> expectEnd [
	self error: 'Unexpected token: ',self asString
]

{ #category : 'testing' }
ASToken >> isLiteral [
	^ false
]

{ #category : 'printing' }
ASToken >> printOn: s [
	s << $< << value asString << $>

]

{ #category : 'parsing' }
ASToken >> processKeyword: anASCompiler target: anASLiteral [ 
	^ nil
]

{ #category : 'accessing' }
ASToken >> symbol [
	^ value asSymbol
]

{ #category : 'accessing' }
ASToken >> value [
	^ value
]

{ #category : 'accessing' }
ASToken >> value: aString [
	value := aString.

]
