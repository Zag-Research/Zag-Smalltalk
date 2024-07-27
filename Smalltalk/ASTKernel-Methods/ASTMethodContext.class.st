Class {
	#name : 'ASTMethodContext',
	#superclass : 'ASTContext',
	#instVars : [
		'method',
		'closureOrNil',
		'receiver'
	],
	#category : 'ASTKernel-Methods',
	#package : 'ASTKernel-Methods'
}

{ #category : 'as yet unclassified' }
ASTMethodContext >> asContext [
	^ self
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> home [
	"Answer the context in which the receiver was defined."

	closureOrNil == nil
		ifTrue: [ ^ self ].
	^ closureOrNil outerContext home
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> method [
	^ method
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> privRefresh [
	"Reinitialize the receiver so that it is in the state it was at its creation."

	closureOrNil
		ifNotNil: [ pc := closureOrNil startpc.
			self stackp: closureOrNil numArgs + closureOrNil numCopiedValues.
			1 to: closureOrNil numCopiedValues do:
				[ :i | self tempAt: closureOrNil numArgs + i put: (closureOrNil at: i) ] ]
		ifNil: [ pc := method initialPC.
			self stackp: method numTemps.
			method numArgs + 1 to: method numTemps do: [ :i | self tempAt: i put: nil ] ]
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> removeSelf [
	"Nil the receiver pointer and answer its former value."

	| result |
	result := receiver.
	receiver := nil.
	^ result
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> setSender: s receiver: r method: m closure: c startpc: startpc [
	"Create the receiver's initial state."

	sender := s.
	receiver := r.
	method := m.
	closureOrNil := c.
	pc := startpc.
	stackp := 0
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> stackp: newStackp [
	"Storing into the stack pointer is a potentially dangerous thing.
	This primitive stores nil into any cells that become accessible as a result,
	and it performs the entire operation atomically."

	"Once this primitive is implemented, failure code should cause an error"

	<primitive: 76>
	self error: 'stackp store failure'
]

{ #category : 'as yet unclassified' }
ASTMethodContext >> tempAt: index put: value [
	"Store the argument, value, as the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at:put: primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	
]
