"
I represent a block of code.

"
Class {
	#name : #ASCodeBlock,
	#superclass : #Object,
	#instVars : [
		'body',
		'locals',
		'scope'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASCodeBlock >> = other [

	^ self class = other class and: [ 
		locals = other locals and: [ body = other body ] ]
]

{ #category : #visiting }
ASCodeBlock >> acceptVisitor: aVisitor [
	self subclassResponsibility 
]

{ #category : #accessing }
ASCodeBlock >> addStatement: anASStatement [

	body addStatement: anASStatement
]

{ #category : #accessing }
ASCodeBlock >> body [
	^ body
]

{ #category : #accessing }
ASCodeBlock >> body: anASSequence [
	body := anASSequence
]

{ #category : #'instance creation' }
ASCodeBlock >> buildClosureData: variables [
	clsrData := OrderedCollection new.
	clsrData add: #footer.
	clsrData addAll: variables
]

{ #category : #'instance creation' }
ASCodeBlock >> closureData [
	^clsrData
]

{ #category : #accessing }
ASCodeBlock >> filter: instructions [
	self subclassResponsibility 
]

{ #category : #initialization }
ASCodeBlock >> initialize [

	super initialize.
	locals := #(  ).
	scope := Dictionary new.
	body := ASSequence new
]

{ #category : #testing }
ASCodeBlock >> isASRef [
	^false
]

{ #category : #testing }
ASCodeBlock >> isASVariable [
	^ false
]

{ #category : #accessing }
ASCodeBlock >> locals [

	^ locals
]

{ #category : #accessing }
ASCodeBlock >> locals: aCollection [

	locals := aCollection
		          collect: [ :symbol | 
			          | local |
			          local := ASLocal name: symbol.
			          scope at: symbol put: local.
			local ]
		          as: Array
]

{ #category : #scopes }
ASCodeBlock >> lookup: symbol [
	^ scope at: symbol ifAbsent: [ nestedScopeOf lookup: symbol ]
]

{ #category : #scopes }
ASCodeBlock >> nestedScopeOf: anASClassNode [ 
	nestedScopeOf := anASClassNode
]

{ #category : #printing }
ASCodeBlock >> printOn: s [

	locals isEmpty ifFalse: [ 
		s << ' locals: #('.
		locals do: [ :local | s << local name ] separatedBy: [ s << ' ' ].
		s<<')' ].
	s << ' body: {'.
	body printOn: s.
	s << '}'
]

{ #category : #accessing }
ASCodeBlock >> scope [
	^ scope
]

{ #category : #writing }
ASCodeBlock >> statementVisitor: symbols stackVars: stackVariables [

	^ self subclassResponsibility 
]

{ #category : #writing }
ASCodeBlock >> writeBody: symbols stackVars: stackVariables codeGenerator: codeGenerator blocks: anASBlockVisitor [

	| statementVisitor |
	anASBlockVisitor
		initializeClosureData: codeGenerator
		stackVariables: stackVariables.

	statementVisitor := self
		                    statementVisitor: symbols
		                    stackVars: stackVariables.
	statementVisitor codeGenerator: codeGenerator.

	body acceptVisitor: statementVisitor.

	"check to see what types variables have"
	(self filter: statementVisitor instructionStack) do: [ :instruction |
		codeGenerator emitInstruction: instruction ]
]

{ #category : #writing }
ASCodeBlock >> writeBody: symbols stackVars: stackVariables writer: aStream blocks: anASBlockVisitor [

	| statementVisitor codeGenerator |
	anASBlockVisitor
		initializeClosureData: aStream
		stackVariables: stackVariables.

	aStream cr.
	codeGenerator := ASZigCompileMethodOutput stream: aStream.
	statementVisitor := self
		                    statementVisitor: symbols
		                    stackVars: stackVariables.
	statementVisitor codeGenerator: codeGenerator.

	body acceptVisitor: statementVisitor.

	"check to see what types variables have"
	(self filter: statementVisitor instructionStack) do: [ :instruction |
		aStream tab.
		aStream << instruction.
		aStream cr ]
]
