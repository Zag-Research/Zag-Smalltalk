"
I'm a second class to define methods for tests
"
Class {
	#name : 'ASCompileTestClass2',
	#superclass : 'ASCompileTestClass1',
	#category : 'Zag-Compiler-Tests-Code',
	#package : 'Zag-Compiler-Tests',
	#tag : 'Code'
}

{ #category : 'testing - examples' }
ASCompileTestClass2 >> classVar [
	^ super classVar negated
]
