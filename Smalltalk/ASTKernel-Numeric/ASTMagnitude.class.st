Class {
	#name : 'ASTMagnitude',
	#superclass : 'ASTObject',
	#category : 'ASTKernel-Numeric',
	#package : 'ASTKernel-Numeric'
}

{ #category : 'comparing' }
ASTMagnitude >> < aMagnitude [
	"Answer whether the receiver is less than the argument."

	^ self subclassResponsibility
]

{ #category : 'comparing' }
ASTMagnitude >> <= aMagnitude [
	"Answer whether the receiver is less than or equal to the argument."

	^ (self > aMagnitude) not
]

{ #category : 'comparing' }
ASTMagnitude >> = aMagnitude [
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^ self subclassResponsibility
]

{ #category : 'comparing' }
ASTMagnitude >> > aMagnitude [
	"Answer whether the receiver is greater than the argument."

	^ aMagnitude < self
]

{ #category : 'comparing' }
ASTMagnitude >> >= aMagnitude [
	"Answer whether the receiver is greater than or equal to the argument."

	^ (self < aMagnitude) not
]

{ #category : 'comparing' }
ASTMagnitude >> between: min and: max [
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^ self >= min and: [ self <= max ]
]

{ #category : 'comparing' }
ASTMagnitude >> hash [
	"Hash must be redefined whenever = is redefined."

	^ self subclassResponsibility
]

{ #category : 'comparing' }
ASTMagnitude >> max: aMagnitude [
	"Answer the receiver or the argument, whichever has the greater 
	magnitude."

	self > aMagnitude
		ifTrue: [ ^ self ]
		ifFalse: [ ^ aMagnitude ]
]

{ #category : 'comparing' }
ASTMagnitude >> min: aMagnitude [
	"Answer the receiver or the argument, whichever has the lesser 
	magnitude."

	self < aMagnitude
		ifTrue: [ ^ self ]
		ifFalse: [ ^ aMagnitude ]
]
