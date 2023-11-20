Class {
	#name : #ASTTemps,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#category : 'ASTSmalltalk-IR'
}

{ #category : #initializing }
ASTTemps >> from: anIRTemps [

	name := anIRTemps name
]
