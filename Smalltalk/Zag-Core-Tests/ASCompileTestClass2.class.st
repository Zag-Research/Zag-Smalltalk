"
I'm a second class to define methods for tests
"
Class {
	#name : 'ASCompileTestClass2',
	#superclass : 'ASCompileTestClass1',
	#category : 'Zag-Core-Tests-Code',
	#package : 'Zag-Core-Tests',
	#tag : 'Code'
}

{ #category : 'testing - examples' }
ASCompileTestClass2 >> classVar [
	^ super classVar negated
]
