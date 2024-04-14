Class {
	#name : 'ASTPushTemp',
	#superclass : 'ASTTemps',
	#category : 'ASTSmalltalk-IR',
	#package : 'ASTSmalltalk',
	#tag : 'IR'
}

{ #category : 'thread' }
ASTPushTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.pushTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
