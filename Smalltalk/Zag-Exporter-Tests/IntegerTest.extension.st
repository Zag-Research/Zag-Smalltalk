Extension { #name : 'IntegerTest' }

{ #category : '*Zag-Exporter-Tests' }
IntegerTest >> testRotate [ 

	self assert: (2r00001100 bitSize: 8 rotate: 5) equals: 2r10000001.
	self assert: (2r00001100 bitSize: 8 rotate: -3) equals: 2r10000001.
	self assert: (2r00011 bitSize: 5 rotate: -1) equals: 2r10001.
	self assert: (2r00001011 bitSize: 8 rotate: -2) equals: 2r11000010.
	self assert: (2r10110000 bitSize: 8 rotate: 3) equals: 2r10000101
]
