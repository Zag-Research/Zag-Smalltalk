"
I contain methods that are used as exemplars for tests.
"
Class {
	#name : #ASCompileTestClass1,
	#superclass : #Object,
	#instVars : [
		'bar'
	],
	#classVars : [
		'Foo'
	],
	#category : #'ASTSmalltalk-Tests-Code'
}

{ #category : #starting }
ASCompileTestClass1 class >> start42 [
	^ 42
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> + n1 [
	| temp |
	temp := self + n1.
	^ temp
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> block [

	^ { 
		  (3 + 4).
		  4 factorial } collect: [ :each | |temp| temp := each + 5. temp + 3 ]
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> cascade [

	| foo |
	foo := #( abc nil 3 ) asOrderedCollection.
	(foo
		 add: 4 negated;
		 yourself)
		add: 3 negated + 4 abs;
		size;
		negated;
		+ 4;
		+ 3 negated;
		abs
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> classVar [

	^ Foo
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> gives43 [
	| forty2 |
	forty2 := self unaries * 2.
	^ self plus1: (forty2 - 4)
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> instVar [

	^ bar
]

{ #category : #'testing - examples' }
ASCompileTestClass1 >> object [

^ Object new
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
