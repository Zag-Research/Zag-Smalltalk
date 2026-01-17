Extension { #name : 'OCVariableNode' }

{ #category : '*Zag-Exporter-Tests' }
OCVariableNode >> zagClosureArgClass: anObject [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCVariableNode >> zagClosureArgClass: argument assign: targetVariable [

	argument = variable ifTrue: [ ^ targetVariable zagBlockAssign ].
	^ #BlockClosure:
]

{ #category : '*Zag-Exporter-Tests' }
OCVariableNode >> zagClosureClass [

	^ #ThunkInstance
]

{ #category : '*Zag-Exporter-Tests' }
OCVariableNode >> zagClosureReturnClass [

	^ variable zagClosureReturnClass
]
