Class {
	#name : #ASCompileTestClass1,
	#superclass : #Object,
	#category : #'ASTSmalltalk-Tests-Code'
}

{ #category : #'testing - examples' }
ASCompileTestClass1 >> gives43 [
	^ self plus1: self simple1
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> plus1: aNumber [
	^ aNumber + 1
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> prim [
	<primitive: 1>
	^ false
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> simple1 [
	^ 42
]
