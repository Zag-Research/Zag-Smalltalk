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
	" self initialize inspect "
	| b s size empty noIvarsOrIndex ivarsAndIndex justIndexable |
	Info := Dictionary new.
	Info at: #symbolCount put: ByteSymbol allInstances size "90813".
	b:= Bag new.
	s:=Set new.
	size := 0.
	empty := Set new.
	noIvarsOrIndex := Set new.
	ivarsAndIndex := Set new.
	justIndexable := Set new.
	Object withAllSubclasses do: [: class |
		b add: ((class methodDict size+1) log*10) asInteger.
		(class class methodDict size = 0 and: [class methodDict size = 0]) ifTrue: [ empty add: class].
		s addAll: class methodDict keys. s addAll: class class methodDict keys.
		class isVariable ifTrue: [
			class instSize isZero ifTrue: [ justIndexable add: class ] ifFalse: [ ivarsAndIndex add: class ].
		] ifFalse: [
			class instSize isZero ifTrue: [ noIvarsOrIndex add: class ].
		].
		size := size + 1.
	].
	Info at: #classCount put: size.
	Info at: #methodDefinitionsPerClass put: b valuesAndCounts associations sorted.
	Info at: #ObjectMethods put:	Object methodDict size.
	Info at: #classesWithNoMethods put: empty asArray sorted.
	Info at: #implementedMethods put: s asArray sorted.
	Info at: #noIvarsOrIndex put: noIvarsOrIndex asArray sorted.
	Info at: #ivarsAndIndex put: ivarsAndIndex asArray sorted.
	Info at: #justIndexable put: justIndexable asArray sorted.
	^ Info
]
