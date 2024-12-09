"
I keep track of potential inlining opportunities
"
Class {
	#name : 'ASInliner',
	#superclass : 'Object',
	#instVars : [
		'implementors'
	],
	#classInstVars : [
		'map',
		'primitiveChecker',
		'primitiveReturn'
	],
	#category : 'ASTSmalltalk-Inlining',
	#package : 'ASTSmalltalk',
	#tag : 'Inlining'
}

{ #category : 'inlining' }
ASInliner class >> alwaysValid: aCompiledMethod returns: returnClass [

	self
		prim: aCompiledMethod primitive
		valid: [ :stack | true ]
		returns: returnClass
]

{ #category : 'inlining' }
ASInliner class >> alwaysValid: aClass selector: selector returns: resultClass [

	self alwaysValid:
		(aClass whichClassIncludesSelector: selector) >> selector returns: resultClass
]

{ #category : 'class initialization' }
ASInliner class >> bothMatch: aCompiledMethod returns: returnClass [

	| class |
	class := aCompiledMethod methodClass.
	self
		prim: aCompiledMethod primitive
		class: class
		parameterClass: class
		returns: returnClass
]

{ #category : 'as yet unclassified' }
ASInliner class >> filterMethods: aCollection [

	| zagOverridden |
	zagOverridden := Set new.
	aCollection do: [ :method |
		| cName |
		cName := method methodClass name.
		(cName beginsWith: 'AST') ifTrue: [
			zagOverridden add: (cName allButFirst: 3) ] ].
	^ aCollection select: [ :method |
		method isRequired not and: [
		  | cName |
		  cName := method methodClass name.
		  (cName beginsWith: 'AST') or: [ (zagOverridden includes: cName) not ] ] ]
]

{ #category : 'inlining' }
ASInliner class >> findImplementations: selector [

	| implementors |
	implementors := self filterMethods: selector implementors.
	(implementors size between: 1 and: 4) ifFalse: [ ^ nil ].
	^ ASInlineNotExhaustive new methods: implementors
]

{ #category : 'inlining' }
ASInliner class >> findImplementations: aSymbol class: aClass stack: aStack [

	aClass ifNil: [ ^ self findImplementations: aSymbol ].
	(aClass whichClassIncludesSelector: aSymbol) ifNotNil: [ :class |
		| astClass |
		astClass := class astClass.
		(astClass includesSelector: aSymbol) ifFalse: [ astClass := class ].
		^ ASInlineSingleton method: astClass >> aSymbol class: aClass ].
	^ nil
]

{ #category : 'class initialization' }
ASInliner class >> initialize [

	self reset
]

{ #category : 'compiling' }
ASInliner class >> lookup: aString [

	^ self lookup: aString class: nil
]

{ #category : 'inlining' }
ASInliner class >> lookup: aSymbol class: aClass [

	^ map
		  at: aClass -> aSymbol
		  ifAbsentPut: [
		  self findImplementations: aSymbol class: aClass stack: nil ]
]

{ #category : 'inlining' }
ASInliner class >> lookup: selector stack: aStack [

	((self
		  lookup: selector
		  class: (aStack at: selector numArgs) dataClass) validate: aStack)
		ifNotNil: [ :v | ^ v ].
	^ nil
]

{ #category : 'instance creation' }
ASInliner class >> new [

	self error: 'don''t create ASInliner instances'
]

{ #category : 'inlining' }
ASInliner class >> prim: anInteger class: selfClass parameterClass: parameterClass returns: returnClass [

	self
		prim: anInteger
		valid: [ :stack |
			stack dataClass = parameterClass and: [
				stack next dataClass = selfClass ] ]
		returns: returnClass
]

{ #category : 'class initialization' }
ASInliner class >> prim: primitive valid: aBlock returns: returnClass [

	[ primitive > primitiveChecker size ] whileTrue: [
		primitiveChecker add: [ :stack | false ].
		primitiveReturn add: nil ].
	primitiveChecker at: primitive put: aBlock.
	primitiveReturn at: primitive put: returnClass
]

{ #category : 'class initialization' }
ASInliner class >> reset [

	(map := LRUCache new) maximumWeight: 100.
	primitiveChecker := OrderedCollection new.
	primitiveReturn := OrderedCollection new.
	self bothMatch: SmallInteger >> #+ returns: SmallInteger.
	" %5 = call { i64, i1 } @llvm.sadd.with.overflow.i64(i64 %4, i64 1), !nosanitize !8
  %6 = extractvalue { i64, i1 } %5, 0, !nosanitize !8
  %7 = extractvalue { i64, i1 } %5, 1, !nosanitize !8
  br i1 %7, label %8, label %10, !prof !9, !nosanitize !8

; <label>:8:                                      ; preds = %0
 !!! error
10: success
"
	self bothMatch: SmallInteger >> #- returns: SmallInteger.
	self bothMatch: SmallInteger >> #+ returns: SmallInteger.
	self bothMatch: SmallInteger >> #< returns: Boolean.
	self bothMatch: SmallInteger >> #'<=' returns: Boolean.
	self bothMatch: SmallInteger >> #= returns: Boolean.
	self bothMatch: SmallInteger >> #'>=' returns: Boolean.
	self bothMatch: SmallInteger >> #> returns: Boolean.
	self alwaysValid: Object selector: #'==' returns: Boolean.
	self alwaysValid: Object selector: #'~~' returns: Boolean.
"	self selfMatch: ASTBlockClosure >> #value.
	self selfMatch: ASTBlockClosure >> #value:.
	self selfMatch: ASTBlockClosure >> #value:value:.
	self selfMatch: ASTBlockClosure >> #value:value:value:.
	self selfMatch: ASTBlockClosure >> #value:value:value:value:.
	self selfMatch: ASTSymbol >> #value:.
	self selfMatch: ASTSymbol >> #value:value:.
	self selfMatch: ASTSymbol >> #value:value:value:.
	self selfMatch: ASTSymbol >> #value:value:value:value:"
]

{ #category : 'inlining' }
ASInliner class >> selfMatch: aCompiledMethod [

	self
		selfMatch: aCompiledMethod primitive
		class: aCompiledMethod methodClass
		numArgs: aCompiledMethod numArgs
]

{ #category : 'inlining' }
ASInliner class >> selfMatch: anInteger class: aClass numArgs: numArgs [

	numArgs = 0 ifTrue: [
		self
			prim: anInteger
			valid: [ :stack | stack dataClass astClass = aClass ]
			returns: nil ].
	numArgs = 1 ifTrue: [
		self
			prim: anInteger
			valid: [ :stack | stack next dataClass astClass = aClass ]
			returns: nil ].
	numArgs = 2 ifTrue: [
		self
			prim: anInteger
			valid: [ :stack | stack next next dataClass astClass = aClass ]
			returns: nil ].
	numArgs = 3 ifTrue: [
		self
			prim: anInteger
			valid: [ :stack | stack next next next dataClass astClass = aClass ]
			returns: nil ].
	numArgs = 4 ifTrue: [
		self
			prim: anInteger
			valid: [ :stack | stack next next next next dataClass astClass = aClass ]
			returns: nil ]
]

{ #category : 'inlining' }
ASInliner class >> validatorFor: anInteger [

	^ primitiveChecker at: anInteger 
]

{ #category : 'initialization' }
ASInliner >> initialize [

	super initialize.
	implementors := Dictionary new.
	implementors at: #not put: {
			(True >> #not).
			(False >> #not) }.
	implementors at: #ifTrue: put: {
			(True >> #ifTrue:).
			(False >> #ifTrue:) }.
	implementors at: #ifFalse: put: {
			(True >> #ifFalse:).
			(False >> #ifFalse:) }.
	implementors at: #ifTrue:ifFalse: put: {
			(True >> #ifTrue:ifFalse:).
			(False >> #ifTrue:ifFalse:) }.
	implementors at: #ifFalse:ifTrue: put: {
			(True >> #ifFalse:ifTrue:).
			(False >> #ifFalse:ifTrue:) }.
	implementors at: #isLarge put: { (SmallInteger >> #isLarge) }.
	implementors at: #not put: { (Number >> #abs) }.
"	implementors at: #value put: { (ASTBlockClosure >> #value) }.
	implementors at: #whileTrue put: { (ASTBlockClosure >> #whileTrue) }.
"	implementors
		at: #fibonacci_fast
		put: { (Integer >> #fibonacci_fast) }.
	implementors
		at: #fibonacci_accumulator:prev:
		put: { (Integer >> #fibonacci_accumulator:prev:) }
]
