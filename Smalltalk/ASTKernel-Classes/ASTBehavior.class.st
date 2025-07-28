Class {
	#name : 'ASTBehavior',
	#superclass : 'ASTObject',
	#instVars : [
		'superclass',
		'methodDict',
		'format'
	],
	#category : 'ASTKernel-Classes',
	#package : 'ASTKernel-Classes'
}

{ #category : 'as yet unclassified' }
ASTBehavior >> >> aSelector [
	^ self methodDict at: aSelector
]

{ #category : 'as yet unclassified' }
ASTBehavior >> allInstVarNames [
	"Answer an Array of the names of all my instance variables."

	^ (1 to: self instSize) collect: [ :i | 'instVar' , i printString ]
]

{ #category : 'as yet unclassified' }
ASTBehavior >> allInstances [
	"Answer a collection of all current instances of the receiver."

	| all |
	all := ASTOrderedCollection new.
	self
		allInstancesDo: [ :x | 
			x == all
				ifFalse: [ all add: x ] ].
	^ all asArray
]

{ #category : 'as yet unclassified' }
ASTBehavior >> allInstancesDo: aBlock [
	"Evaluate the given block for each instance of the receiver."

	| inst |
	self == ASTUndefinedObject
		ifTrue: [ ^ aBlock value: nil ].
	inst := self someInstance.
	[ inst == nil ]
		whileFalse: [ aBlock value: inst.
			inst := inst nextInstance ]
]

{ #category : 'reflective operations' }
ASTBehavior >> basicIdentityHash [
	"Answer a 16 bits unsigned SmallInteger, whose value is related to the receiver's identity
	 and unique among the behaviors (i.e. 2 different Behaviors cannot have the same identityHash).

	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  Using
	 this implementation classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.  In Zag as of
	 2023 there are 16 bits of classTable index and 16 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 2075>
	self primitiveFailed
]

{ #category : 'as yet unclassified' }
ASTBehavior >> basicNew [
	"Primitive. Answer an instance of the receiver (which is a class) with no indexable fields. Fail if the class is variable-sized. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	self isVariable
		ifTrue: [ ^ self basicNew: 0 ].	"space must be low"
	self error: 'low space'
]

{ #category : 'as yet unclassified' }
ASTBehavior >> basicNew: desiredSize [
	"Primitive. Answer an instance of this class with the given number of indexable variables. Fail if this class is not indexable, if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	self isVariable
		ifFalse: [ self error: self name , ' is not variable-sized' ].
	(desiredSize isInteger not or: [ desiredSize < 0 ])
		ifTrue: [ self error: 'bad size' ].
	self error: 'low space'
]

{ #category : 'as yet unclassified' }
ASTBehavior >> canUnderstand: selector [
	"Answer whether the receiver can respond to the message whose selector is the argument. The selector can be in the method dictionary of the receiver's class or any of its superclasses."

	^ (self lookupSelector: selector) ~~ nil
]

{ #category : 'as yet unclassified' }
ASTBehavior >> classPool [
	^ nil
]

{ #category : 'as yet unclassified' }
ASTBehavior >> format [
	"Answer a integer that encodes the format of instances of this class receiver."

	^ format
]

{ #category : 'as yet unclassified' }
ASTBehavior >> indexIfCompact [
	"If my compact class index is non-zero, then instances of this class will be compact, and their class will have an entry in Smalltalk compactClassesArray."

	^ (format bitShift: -11) bitAnd: 16r1F
]

{ #category : 'as yet unclassified' }
ASTBehavior >> inheritsFrom: aClass [
	"Answer whether the argument is on the receiver's superclass chain."

	| this |
	this := superclass.
	[ this == nil ]
		whileFalse: [ this == aClass
				ifTrue: [ ^ true ].
			this := this superclass ].
	^ false
]

{ #category : 'as yet unclassified' }
ASTBehavior >> initialize [
	"Default initialization."

	superclass := ASTObject.
	format := 2.
	methodDict := ASTMethodDictionary new
]

{ #category : 'as yet unclassified' }
ASTBehavior >> instSize [
	"Answer the number of named instance variables of the receiver. Objects can have up to 255 instance variables."

	^ ((format bitShift: -10) bitAnd: 16rC0)
		+ ((format bitShift: -1) bitAnd: 16r3F) - 1
]

{ #category : 'as yet unclassified' }
ASTBehavior >> instSpec [
	^ (format bitShift: -7) bitAnd: 16rF
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isBehavior [
	"Answer true if I am a subclass of Behavior."

	^ true
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isBits [
	"Answer whether the receiver contains just bits (not pointers)."

	^ self instSpec >= 6
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isBytes [
	"Answer true if the receiver is byte-indexable. This is always false for non-indexable and pointer objects."

	^ self instSpec >= 8
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isCompact [
	^ self indexIfCompact ~= 0
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isPointers [
	"Answer true if the receiver contains object pointers (versus bytes or words)."

	^ self isBits not
]

{ #category : 'as yet unclassified' }
ASTBehavior >> isVariable [
	"Answer true if the receiver has indexable fields."

	^ self instSpec >= 2
]

{ #category : 'as yet unclassified' }
ASTBehavior >> lookupSelector: selector [
	"Look up the given selector in the methodDictionaries of every class in my superclass chain and answer the corresponding method if found. Answer nil if no method is found."

	| lookupClass mDict |
	lookupClass := self.
	[ lookupClass == nil ]
		whileFalse: [ mDict := lookupClass methodDict.
			(mDict includesKey: selector)
				ifTrue: [ ^ mDict at: selector ].
			lookupClass := lookupClass superclass ].
	^ nil
]

{ #category : 'as yet unclassified' }
ASTBehavior >> methodDict [
	"Answer my method dictionary."

	^ methodDict
]

{ #category : 'as yet unclassified' }
ASTBehavior >> name [
	"Answer a String that names the receiver as well possible. Overridden by my subclasses."

	superclass
		ifNil: [ ^ 'a subclass of nil' ]
		ifNotNil: [ ^ 'a subclass of ' , superclass name ]
]

{ #category : 'instance creation' }
ASTBehavior >> new [
	^ self basicNew
]

{ #category : 'as yet unclassified' }
ASTBehavior >> new: desiredSize [
	^ self basicNew: desiredSize
]

{ #category : 'printing' }
ASTBehavior >> printOn: aStream [
	aStream nextPutAll: self name
]

{ #category : 'as yet unclassified' }
ASTBehavior >> selectorAtMethod: method setClass: classResultBlock [
	"Answer the message selector associated with the compiled method and evaluate the classResultBlock on the class in which that selector is defined."

	"Note: This method is here only to support the debugger."

	| sel |
	sel := self methodDict
		keyAtIdentityValue: method
		ifAbsent: [ superclass
				ifNil: [ classResultBlock value: self.
					^ #DoIt ].
			sel := superclass
				selectorAtMethod: method
				setClass: classResultBlock.
			sel == #DoIt
				ifTrue: [ classResultBlock value: self ].	"if selector is DoIt, set class to me"
			^ sel ].
	classResultBlock value: self.
	^ sel
]

{ #category : 'as yet unclassified' }
ASTBehavior >> setCompactClassIndex: ccIndex [
	"If my compact class index is non-zero, then instances of this class will be compact, and their class will have an entry in Smalltalk compactClassesArray."

	(ccIndex > 0 and: [ ccIndex <= 31 ])
		ifFalse: [ ^ self error: 'compact class index must 1-31' ].
	self indexIfCompact = 0
		ifFalse: [ ^ self error: self name , ' is already compact!' ].
	format := format + (ccIndex bitShift: 11)
]

{ #category : 'as yet unclassified' }
ASTBehavior >> setFormat: anInteger [
	"Warning! Risky operation. Should only be done when there are not instances of this class."

	format := anInteger
]

{ #category : 'as yet unclassified' }
ASTBehavior >> sharedPools [
	^ nil
]

{ #category : 'as yet unclassified' }
ASTBehavior >> someInstance [
	"Primitive. Answer the first instance in the enumeration of all instances of the receiver. Answer nil if there are none. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^ nil
]

{ #category : 'as yet unclassified' }
ASTBehavior >> superclass [
	"Answer my superclass."

	^ superclass
]

{ #category : 'as yet unclassified' }
ASTBehavior >> superclass: aBehaviorOrNil [
	"Set my superclass."

	superclass := aBehaviorOrNil
]
