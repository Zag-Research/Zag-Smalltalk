Class {
	#name : 'ZagSubstitutions',
	#superclass : 'Object',
	#classInstVars : [
		'substitutionMap'
	],
	#category : 'Zag-Substitutions',
	#package : 'Zag-Substitutions'
}

{ #category : 'adding' }
ZagSubstitutions class >> addMappingsTo: map [

	self methods do: [ : method | map at: (self mirroredClass methodNamed: method selector) put: method ]
]

{ #category : 'class initialization' }
ZagSubstitutions class >> initialize [

	substitutionMap := self substitutionMap
]

{ #category : 'as yet unclassified' }
ZagSubstitutions class >> mirroredClass [
	self subclassResponsibility 
]

{ #category : 'as yet unclassified' }
ZagSubstitutions class >> substitutionMap [
 " self substitutionMap "

	| map |
	map := IdentityDictionary new.
	self subclassesDo: [ :class | class addMappingsTo: map ].
	^ map
]
