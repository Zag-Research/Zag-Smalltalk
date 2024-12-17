Extension { #name : 'IRPushInstVar' }

{ #category : '*Zag-Core' }
IRPushInstVar >> threadFormatOn: anASTThreadFormat [

	anASTThreadFormat
		nextPutAll: 'p.pushInstVar,Code.int(';
		nextPutAll: self index asString;
		nextPutAll: '),'.
	^ nil
]
