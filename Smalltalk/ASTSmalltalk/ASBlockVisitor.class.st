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
	#category : #'ASTSmalltalk-Visitors'
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

{ #category : #'as yet unclassified' }
ASBlockVisitor >> blockClosureForVar: anASVar [

	| var |
	self halt.
	var := variables at: anASVar name
	"var modifiers ifEmpty:  [] ifNotEmpty: [ ]"
]

{ #category : #accessing }
ASBlockVisitor >> blocks [
	^ blocks
]

{ #category : #accessing }
ASBlockVisitor >> closures [

	| closureNames |
	closureNames := OrderedCollection new.
	self modifiers ifEmpty: [ blocks do:[ :b | b buildClosureData: self modifiers ] ] ifNotEmpty: [ "add closureData to the names and to each block"
		closureNames add: #closureData.
		"add a closureData to each blocks closure data"
		blocks do:[ :b | b buildClosureDataWithModifiers: self modifiers ]  ].
	
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

{ #category : #visiting }
ASBlockVisitor >> initialize [

	blocks := OrderedCollection new.
	variables := Dictionary new.
	index := 0
]

{ #category : #initialization }
ASBlockVisitor >> initializeClosureData: codeGenerator stackVariables: stackVariables [

	variables do: [ :var |
		var isASArg ifTrue: [
			var references do: [ :blockRef |
				codeGenerator pushLocal: var name.
				codeGenerator
					popLocalData: var name
					stackIndex:
					(stackVariables indexOf: blockRef selector asZigString) - 1
					blockIndex: (blockRef location: var name) ] ] ]
]

{ #category : #accessing }
ASBlockVisitor >> modifiers [

	| modifiers |
	modifiers := Set new.
	blocks do: [ :b | modifiers addAll: (b modifiers collect: [ :m | m name ]) ].
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

	

	anASBlock isNonLocalReturn
		ifTrue: [ block ifNotNil: [ block includeContext ] ]
		ifFalse: [ | parentBlock |
	parentBlock := block.
	block := anASBlock.
			block selector: selector , ':' , self nextBlockPostfixIndex.
			anASBlock body ifNotEmpty: [
				blocks add: anASBlock.
				anASBlock body do: [ :statement | statement acceptVisitor: self ] ].
			block := parentBlock ].
	
]

{ #category : #visiting }
ASBlockVisitor >> visitClassVar: anASClassVar [
	"ingnore class variables for now"
]

{ #category : #writing }
ASBlockVisitor >> visitClosureData: codeGenerator stackVariables: stackVariables [

	self modifiers ifNotEmpty: [ "write the closure data instruction to the stream"
		codeGenerator closure: ((stackVariables indexOf: #closureData) - 1) modifier:  self modifiers size]
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
	block ifNotNil: [ 
		block includeContext ].
	anASReturn expression acceptVisitor: self
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
			self block: block references: anASRef variable.
			block references: anASRef variable ]
]

{ #category : #writing }
ASBlockVisitor >> writeClosureData: aStream stackVariables: stackVariables [

	self modifiers ifNotEmpty: [ "write the closure data instruction to the stream"
		aStream tab.
		aStream << '&e.closureData, '
		<< ((stackVariables indexOf: #closureData) - 1) asString << ' + ('
		<< self modifiers size asString
		<< ' << 8), // local:3 size:1 (offset 1 is l1)'.
		aStream cr ]
]
