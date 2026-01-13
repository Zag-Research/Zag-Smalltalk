Extension { #name : 'OCAssignmentNode' }

{ #category : '*Zag-Exporter-Tests' }
OCAssignmentNode >> zagClosureArgClass: argument [

	^ value zagClosureArgClass: argument assign: variable variable

]

{ #category : '*Zag-Exporter-Tests' }
OCAssignmentNode >> zagClosureArgClass: anArgumentVariable assign: aVariable [

self halt.
	^ #BlockClosure:variable
]

{ #category : '*Zag-Exporter-Tests' }
OCAssignmentNode >> zagClosureClass [

	^ #BlockClosure:assignment
]

{ #category : '*Zag-Exporter-Tests' }
OCAssignmentNode >> zagClosureReturnClass [

	^ #BlockClosure
]
