Extension { #name : 'OCBlockNode' }

{ #category : '*Zag-Exporter-Tests' }
OCBlockNode >> zagClosureArgClass: anArgumentVariable [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCBlockNode >> zagClosureClass [

	self statements size ~= 1 ifTrue: [
		^ self statements size = 0
			ifTrue: [ #ThunkImmediate ]
			ifFalse: [ #BlockClosure ] ].
	arguments size = 0 ifTrue: [ ^ self statements first zagClosureClass ].
	arguments size = 1 ifTrue: [
		^ self statements first zagClosureArgClass: arguments first variable ].
	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCBlockNode >> zagClosureReturnClass [

	^ #BlockClosure
]
