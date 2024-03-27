Class {
	#name : #ASTPopIntoTemp,
	#superclass : #ASTTemps,
	#category : 'ASTSmalltalk-IR'
}

{ #category : #thread }
ASTPopIntoTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: '&p.popIntoTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
