Extension { #name : 'String' }

{ #category : '*Zag-Core' }
String >> zigName [

	(self allSatisfy: #isAlphaNumeric) ifTrue: [ ^ self asString ].
	^ '@"' , self , '"'
]
