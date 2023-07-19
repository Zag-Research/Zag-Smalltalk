Class {
	#name : #ASBlockVisitor,
	#superclass : #Object,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#instVars : [
		'blocks',
		'block'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASBlockVisitor >> blocks [
	^ blocks
]

{ #category : #visiting }
ASBlockVisitor >> initialize [ 
	blocks := OrderedCollection new
]

{ #category : #accessing }
ASBlockVisitor >> modifiers [
	| modifiers |
	modifiers := Set new.
	blocks do: [ :b | modifiers addAll: b modifiers ].
	^ modifiers
]

{ #category : #visiting }
ASBlockVisitor >> visit: statements [

	statements ifNotEmpty: [
		statements do: [ :stmt | stmt acceptVisitor: self ] ]
]

{ #category : #visiting }
ASBlockVisitor >> visitArg: anASArg [ 
	"don't need to worry about args"
]

{ #category : #visiting }
ASBlockVisitor >> visitAssign: anASAssign [

	anASAssign expression acceptVisitor: self.
	anASAssign variable acceptVisitor: self.
	
	block ifNotNil: [ block modifies: anASAssign variable ]
]

{ #category : #visiting }
ASBlockVisitor >> visitBlockNode: anASBlock [

	
	block := anASBlock.
	anASBlock isNonLocalReturn ifFalse: [
		anASBlock body ifNotEmpty: [
			blocks add: anASBlock.
			anASBlock body do: [ :statement | statement acceptVisitor: self ] ] ].
	block := nil
]

{ #category : #visiting }
ASBlockVisitor >> visitLiteral: anASLiteral [ 
	"ignore literals"
]

{ #category : #visiting }
ASBlockVisitor >> visitNamedAS: anASLocal [ 
	"don't need to worry about locals"
	"may want to collect the locals if block is nil"
]

{ #category : #visiting }
ASBlockVisitor >> visitReturn: anASReturn [
	anASReturn expression acceptVisitor: self.

	
]

{ #category : #visiting }
ASBlockVisitor >> visitSelfNode: anASSelf [ 
	"do nothing"
]

{ #category : #visiting }
ASBlockVisitor >> visitSend: anASSend [
	anASSend target acceptVisitor: self.
	anASSend args ifNotNil: [
		anASSend args do: [ :arg | arg acceptVisitor: self ] ].

	
]

{ #category : #visiting }
ASBlockVisitor >> visitVariable: anASRef [
	block ifNotNil: [ anASRef variable isASArg ifFalse: [ block references: anASRef variable ]]
]
