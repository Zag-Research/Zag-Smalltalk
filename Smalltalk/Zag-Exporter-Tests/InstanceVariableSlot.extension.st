Extension { #name : 'InstanceVariableSlot' }

{ #category : '*Zag-Exporter-Tests' }
InstanceVariableSlot >> zagBlockAssign [

	^ #BlockAssignInstance
]

{ #category : '*Zag-Exporter-Tests' }
InstanceVariableSlot >> zagClosureClass [

	^ #ThunkInstance
]

{ #category : '*Zag-Exporter-Tests' }
InstanceVariableSlot >> zagClosureReturnClass [

	^ #ThunkReturnInstance
]

{ #category : '*Zag-Exporter-Tests' }
InstanceVariableSlot >> zagIsLocal [

	^ false
]
