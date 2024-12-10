"
I'm a third class to define methods for tests
"
Class {
	#name : 'ASCompileTestClass3',
	#superclass : 'Object',
	#instVars : [
		'times'
	],
	#pools : [
		'ASCompileTestClass1'
	],
	#category : 'Zag-Core-Tests-Code',
	#package : 'Zag-Core-Tests',
	#tag : 'Code'
}

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> aCallsB [
| value |
value := self bCallsC + 1.
	Transcript
		show: #calling_aCallsB;
		tab;
		show: value asString;
		cr.
	^ value
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> aCallsB: value [
	Transcript show: #calling_aCallsB.
	^ self bCallsC: value + 1
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> bCallsC [

	^ self cCallsD
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> bCallsC: aValue [

	^self cCallsD: aValue
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> cCallsD [

	times := times + 1.
	times > 20 ifTrue: [ self halt. ^ 0 ].
	^ self aCallsB
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> cCallsD: aValue [

	aValue > 20 ifTrue: [ ^aValue].
	^ self aCallsB: aValue
	
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> initialize [ 
	times :=0
]

{ #category : 'as yet unclassified' }
ASCompileTestClass3 >> pool [
	^ Foo
]
