Extension { #name : 'SmallFloat64' }

{ #category : '*Zag-Exporter-Tests' }
SmallFloat64 >> zagClosureClass [

	self flag: #ToDo. " check that the low byte is 0"
	^ #ThunkFloat
]
