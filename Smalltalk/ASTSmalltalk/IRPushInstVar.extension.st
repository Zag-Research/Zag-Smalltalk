Extension { #name : #IRPushInstVar }

{ #category : #'*ASTSmalltalk' }
IRPushInstVar >> threadFormatOn: anASTThreadFormat [

	anASTThreadFormat
		nextPutAll: 'p.pushInstVar,Code.int(';
		nextPutAll: self index asString;
		nextPutAll: '),'.
	^ nil
]
