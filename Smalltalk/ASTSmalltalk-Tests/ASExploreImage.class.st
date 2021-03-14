Class {
	#name : #ASExploreImage,
	#superclass : #Object,
	#classVars : [
		'Info',
		'Something'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'class initialization' }
ASExploreImage class >> initialize [
	| b p prev s |
	Info := Dictionary new.
	Info at: #symbolCount put: ByteSymbol allInstances size "90813".
b:= Bag new.Object withAllSubclasses do: [: c | b add: ((c methodDict size+1) log*10) asInteger ].b valuesAndCounts associations sorted .
Object withAllSubclasses select: [: c | c class methodDict size = 0 and: [c methodDict size = 0]] .
p:=Integer primesUpTo: 800000.
{prev:=2.p allButFirst collect: [: e | |t| t:=prev.prev:=e.e-t ]} last asBag valuesAndCounts associations sorted.
{s:=Set new.Object withAllSubclasses do: [: c | s addAll: c methodDict keys. s addAll: c class methodDict keys ] } first.
Object methodDict size.
Object withAllSubclasses size.
{ s:=OrderedCollection new. (s add: 1;yourself) add: (s add: 2);add: (s add: 3) } first asArray.

]
