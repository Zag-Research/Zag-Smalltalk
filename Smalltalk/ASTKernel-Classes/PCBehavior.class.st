Class {
	#name : #PCBehavior,
	#superclass : #PCObject,
	#instVars : [
		'superclass',
		'methodDict',
		'format'
	],
	#category : #'ASTKernel-Classes'
}

{ #category : #'as yet unclassified' }
PCBehavior >> >> aSelector [
	^ self methodDict at: aSelector
]

{ #category : #'as yet unclassified' }
PCBehavior >> allInstVarNames [
	"Answer an Array of the names of all my instance variables."

	^ (1 to: self instSize) collect: [ :i | 'instVar' , i printString ]
]

{ #category : #'as yet unclassified' }
PCBehavior >> allInstances [
	"Answer a collection of all current instances of the receiver."

	| all |
	all := PCOrderedCollection new.
	self
		allInstancesDo: [ :x | 
			x == all
				ifFalse: [ all add: x ] ].
	^ all asArray
]

{ #category : #'as yet unclassified' }
PCBehavior >> allInstancesDo: aBlock [
	"Evaluate the given block for each instance of the receiver."

	| inst |
	self == PCUndefinedObject
		ifTrue: [ ^ aBlock value: nil ].
	inst := self someInstance.
	[ inst == nil ]
		whileFalse: [ aBlock value: inst.
			inst := inst nextInstance ]
]

{ #category : #'as yet unclassified' }
PCBehavior >> basicNew [
	"Primitive. Answer an instance of the receiver (which is a class) with no indexable fields. Fail if the class is variable-sized. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 70>
	self isVariable
		ifTrue: [ ^ self basicNew: 0 ].	"space must be low"
	self error: 'low space'
]

{ #category : #'as yet unclassified' }
PCBehavior >> basicNew: desiredSize [
	"Primitive. Answer an instance of this class with the given number of indexable variables. Fail if this class is not indexable, if the argument is not a positive Integer, or if there is not enough memory available. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 71>
	self isVariable
		ifFalse: [ self error: self name , ' is not variable-sized' ].
	(desiredSize isInteger not or: [ desiredSize < 0 ])
		ifTrue: [ self error: 'bad size' ].
	self error: 'low space'
]

{ #category : #'as yet unclassified' }
PCBehavior >> canUnderstand: selector [
	"Answer whether the receiver can respond to the message whose selector is the argument. The selector can be in the method dictionary of the receiver's class or any of its superclasses."

	^ (self lookupSelector: selector) ~~ nil
]

{ #category : #'as yet unclassified' }
PCBehavior >> classPool [
	^ nil
]

{ #category : #'as yet unclassified' }
PCBehavior >> format [
	"Answer a integer that encodes the format of instances of this class receiver."

	^ format
]

{ #category : #'as yet unclassified' }
PCBehavior >> indexIfCompact [
	"If my compact class index is non-zero, then instances of this class will be compact, and their class will have an entry in Smalltalk compactClassesArray."

	^ (format bitShift: -11) bitAnd: 16r1F
]

{ #category : #'as yet unclassified' }
PCBehavior >> inheritsFrom: aClass [
	"Answer whether the argument is on the receiver's superclass chain."

	| this |
	this := superclass.
	[ this == nil ]
		whileFalse: [ this == aClass
				ifTrue: [ ^ true ].
			this := this superclass ].
	^ false
]

{ #category : #'as yet unclassified' }
PCBehavior >> initialize [
	"Default initialization."

	superclass := PCObject.
	format := 2.
	methodDict := PCMethodDictionary new
]

{ #category : #'as yet unclassified' }
PCBehavior >> instSize [
	"Answer the number of named instance variables of the receiver. Objects can have up to 255 instance variables."

	^ ((format bitShift: -10) bitAnd: 16rC0)
		+ ((format bitShift: -1) bitAnd: 16r3F) - 1
]

{ #category : #'as yet unclassified' }
PCBehavior >> instSpec [
	^ (format bitShift: -7) bitAnd: 16rF
]

{ #category : #'as yet unclassified' }
PCBehavior >> isBehavior [
	"Answer true if I am a subclass of Behavior."

	^ true
]

{ #category : #'as yet unclassified' }
PCBehavior >> isBits [
	"Answer whether the receiver contains just bits (not pointers)."

	^ self instSpec >= 6
]

{ #category : #'as yet unclassified' }
PCBehavior >> isBytes [
	"Answer true if the receiver is byte-indexable. This is always false for non-indexable and pointer objects."

	^ self instSpec >= 8
]

{ #category : #'as yet unclassified' }
PCBehavior >> isCompact [
	^ self indexIfCompact ~= 0
]

{ #category : #'as yet unclassified' }
PCBehavior >> isPointers [
	"Answer true if the receiver contains object pointers (versus bytes or words)."

	^ self isBits not
]

{ #category : #'as yet unclassified' }
PCBehavior >> isVariable [
	"Answer true if the receiver has indexable fields."

	^ self instSpec >= 2
]

{ #category : #'as yet unclassified' }
PCBehavior >> lookupSelector: selector [
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

{ #category : #'as yet unclassified' }
PCBehavior >> methodDict [
	"Answer my method dictionary."

	^ methodDict
]

{ #category : #'as yet unclassified' }
PCBehavior >> name [
	"Answer a String that names the receiver as well possible. Overridden by my subclasses."

	superclass
		ifNil: [ ^ 'a subclass of nil' ]
		ifNotNil: [ ^ 'a subclass of ' , superclass name ]
]

{ #category : #'as yet unclassified' }
PCBehavior >> new [
	^ self basicNew
]

{ #category : #'as yet unclassified' }
PCBehavior >> new: desiredSize [
	^ self basicNew: desiredSize
]

{ #category : #'as yet unclassified' }
PCBehavior >> printOn: aStream [
	aStream nextPutAll: self name
]

{ #category : #'as yet unclassified' }
PCBehavior >> selectorAtMethod: method setClass: classResultBlock [
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

{ #category : #'as yet unclassified' }
PCBehavior >> setCompactClassIndex: ccIndex [
	"If my compact class index is non-zero, then instances of this class will be compact, and their class will have an entry in Smalltalk compactClassesArray."

	(ccIndex > 0 and: [ ccIndex <= 31 ])
		ifFalse: [ ^ self error: 'compact class index must 1-31' ].
	self indexIfCompact = 0
		ifFalse: [ ^ self error: self name , ' is already compact!' ].
	format := format + (ccIndex bitShift: 11)
]

{ #category : #'as yet unclassified' }
PCBehavior >> setFormat: anInteger [
	"Warning! Risky operation. Should only be done when there are not instances of this class."

	format := anInteger
]

{ #category : #'as yet unclassified' }
PCBehavior >> sharedPools [
	^ nil
]

{ #category : #'as yet unclassified' }
PCBehavior >> someInstance [
	"Primitive. Answer the first instance in the enumeration of all instances of the receiver. Answer nil if there are none. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^ nil
]

{ #category : #'as yet unclassified' }
PCBehavior >> superclass [
	"Answer my superclass."

	^ superclass
]

{ #category : #'as yet unclassified' }
PCBehavior >> superclass: aBehaviorOrNil [
	"Set my superclass."

	superclass := aBehaviorOrNil
]
