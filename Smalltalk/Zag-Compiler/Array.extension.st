Extension { #name : 'Array' }

{ #category : '*Zag-Compiler' }
Array >> zigWalkEmpty: emptyBlock rest: restBlock last: lastBlock [

	| size |
	size := self size.
	self withIndexDo: [ :elem :idx |
			idx = size
				ifTrue: [ ^ lastBlock value: elem ]
				ifFalse: [ restBlock value: elem ] ].
	^ emptyBlock value
]
