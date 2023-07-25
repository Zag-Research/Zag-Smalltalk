Class {
	#name : #ASBlockVisitor,
	#superclass : #Object,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#instVars : [
		'blocks',
		'block',
		'variables',
		'selector',
		'index'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #constructor }
ASBlockVisitor class >> selector: selector [

	^ self new selector: selector
]

{ #category : #visiting }
ASBlockVisitor >> block: anASBlock modifies: anASVar [ 
	variables at: anASVar name ifPresent: [ :var | var modifiedBy: anASBlock ] ifAbsentPut: [ anASVar modifiedBy: anASBlock. anASVar  ]
]

{ #category : #visiting }
ASBlockVisitor >> block: anASBlock references: anASVar [

	variables at: anASVar name ifPresent: [ :var | var referencedBy: anASBlock ] ifAbsentPut: [ anASVar referencedBy: anASBlock. anASVar  ]
]

{ #category : #accessing }
ASBlockVisitor >> blocks [
	^ blocks
]

{ #category : #accessing }
ASBlockVisitor >> closures [

	| closureNames |
	closureNames := OrderedCollection new.
	self hasModifiers ifTrue: [ closureNames add: 'closureData' ].
	closureNames addAll:
		(blocks collect: [ :b | b selector asZigString ]).
	^ closureNames reverse
]

{ #category : #accessing }
ASBlockVisitor >> complexBlocks [
	self shouldBeImplemented.
]

{ #category : #filter }
ASBlockVisitor >> filterVars: locals [ 
	^ (locals reject: [:l | variables includesKey: l name]) 
]

{ #category : #testing }
ASBlockVisitor >> hasModifiers [
	variables do: [ :var |  var hasModifiers ifTrue: [ ^true ] ].
	^false
]

{ #category : #visiting }
ASBlockVisitor >> initialize [

	blocks := OrderedCollection new.
	variables := Dictionary new.
	index := 0
]

{ #category : #accessing }
ASBlockVisitor >> modifiers [
	| modifiers |
	modifiers := Set new.
	blocks do: [ :b | modifiers addAll: b modifiers ].
	^ modifiers
]

{ #category : #accessing }
ASBlockVisitor >> nextBlockPostfixIndex [
	index := index +1.
	^index asString
]

{ #category : #constructor }
ASBlockVisitor >> selector: name [

	selector := name
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

	block ifNotNil: [
		self block: block modifies: anASAssign variable.
		block modifies: anASAssign variable ]
]

{ #category : #visiting }
ASBlockVisitor >> visitBlockNode: anASBlock [

	block := anASBlock.

	anASBlock isNonLocalReturn ifFalse: [
		block selector: selector , ':' , self nextBlockPostfixIndex.
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

	block ifNotNil: [
		anASRef variable isASArg ifFalse: [
			self block: block references: anASRef variable.
			block references: anASRef variable ] ]
]
