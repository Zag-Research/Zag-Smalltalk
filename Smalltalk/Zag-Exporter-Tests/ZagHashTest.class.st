Class {
	#name : 'ZagHashTest',
	#superclass : 'TestCase',
	#category : 'Zag-Exporter-Tests-Tests',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ZagHashTest >> testHash [

	self assert: 1 bitHash equals: 16r42741D6
]
