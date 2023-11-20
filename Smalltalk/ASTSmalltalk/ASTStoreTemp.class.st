Class {
	#name : 'ASTStoreTemp',
	#superclass : 'ASTTemps',
	#category : 'ASTSmalltalk-IR',
	#package : 'ASTSmalltalk',
	#tag : 'IR'
}

{ #category : 'thread' }
ASTStoreTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.storeTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
