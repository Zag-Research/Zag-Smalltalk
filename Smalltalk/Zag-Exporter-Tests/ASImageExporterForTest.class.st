"
I am an Image exporter for tests that producces repeatable results similar to the real code.
"
Class {
	#name : 'ASImageExporterForTest',
	#superclass : 'ASImageExporter',
	#category : 'Zag-Exporter-Tests-Support',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Support'
}

{ #category : 'accessing' }
ASImageExporterForTest class >> heapClass [

	^ ASSubHeapForTest
]

{ #category : 'image analysis' }
ASImageExporterForTest class >> primitiveNumber: n [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ])
		    select: [ :p | p selector = #primitive: and: [p arguments first = n] ])
]

{ #category : 'image analysis' }
ASImageExporterForTest class >> primitiveNumbers [
^ (((#primitive: senders flatCollect: [ :m| m pragmas]) select: [:p| p selector = #primitive: ] thenCollect: [:p| p arguments first ]) asSet asArray select: #isInteger) asSortedCollection 
]

{ #category : 'image analysis' }
ASImageExporterForTest class >> primitiveNumbers: aCollection [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ]) select: [ :p |
		  p selector = #primitive: and: [
			  aCollection includes: p arguments first ] ]) asSortedCollection: [ :l :r | l arguments first <= r arguments first ]
]
