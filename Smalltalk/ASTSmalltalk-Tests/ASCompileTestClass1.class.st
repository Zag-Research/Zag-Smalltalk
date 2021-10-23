Class {
	#name : #ASCompileTestClass1,
	#superclass : #Object,
	#category : #'ASTSmalltalk-Tests-Code'
}

{ #category : #'testing - examples' }
ASCompileTestClass1 >> + n1 [
	| temp |
	temp := self + n1.
	^ temp
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> cascade [
	| foo |
	foo := #(abc nil 3) asOrderedCollection.
	foo add: 4 negated; add: 3 negated + 4 abs; size; negated; + 4; + 3 negated; abs.

]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> gives43 [
	| forty2 |
	forty2 := self unaries * 2.
	^ self plus1: (forty2 - 4)
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> plus1: aNumber [
	^ aNumber + 1
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> plus: n1 and: n2 and: n3 [
	^ n1 + n2 + n3
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> prim [
	<primitive: 1>
	^ false
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> simple1 [
	..
	^ 42..
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> unaries [
	^ 23 negated abs
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> | n1 [
	| temp |
	temp := self | n1.
	^ temp
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> || n1 [
	| temp temp2 |
	temp := temp2 :=self || n1.
	^ temp
]
