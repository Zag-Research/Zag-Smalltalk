"
I represent a block of code.

"
Class {
	#name : #ASCodeBlock,
	#superclass : #Object,
	#instVars : [
		'nestedScopeOf',
		'body',
		'locals',
		'scope',
		'clsrData'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASCodeBlock >> = other [

	^ self class = other class and: [ 
		locals = other locals and: [ body asArray = other body asArray ] ]
]

{ #category : #accessing }
ASCodeBlock >> addStatement: anASStatement [

	body add: anASStatement
]

{ #category : #accessing }
ASCodeBlock >> body [
	^ body
]

{ #category : #accessing }
ASCodeBlock >> body: anASNode [ 
	body := anASNode
]

{ #category : #'instance creation' }
ASCodeBlock >> buildClosureData: variables [
	clsrData := OrderedCollection new.
	clsrData add: #footer.
	clsrData addAll: variables
]

{ #category : #accessing }
ASCodeBlock >> filter: instructions [
	self subclassResponsibility 
]

{ #category : #initialization }
ASCodeBlock >> initialize [

	super initialize.
	locals := #(  ).
	body := OrderedCollection new.
	scope := Dictionary new
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
	body do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
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
ASCodeBlock >> writeBody: symbols stackVars: stackVariables writer: aStream blocks: anASBlockVisitor [

	| statementVisitor |
	anASBlockVisitor
		initializeClosureData: aStream
		stackVariables: stackVariables.

	aStream cr.
	statementVisitor := self
		                    statementVisitor: symbols
		                    stackVars: stackVariables.

	body do: [ :asStatement |
		asStatement acceptVisitor: statementVisitor.
		asStatement isASSend ifTrue: [ statementVisitor addDrop ] ].
	
	"check to see what types variables have"
	(self filter: statementVisitor instructionStack) do: [ :instruction |
		aStream tab.
		aStream << instruction.
		aStream cr ]
]
