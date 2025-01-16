Class {
	#name : 'ZagSubstitutions',
	#superclass : 'Object',
	#classInstVars : [
		'substitutionMap'
	],
	#category : 'Zag-Substitutions',
	#package : 'Zag-Substitutions'
}

{ #category : 'as yet unclassified' }
ZagSubstitutions class >> addMappingsTo: map [

	self methods do: [ : method | map at: method put: (self mirroredClass methodNamed: method selector) ]
]

{ #category : 'class initialization' }
ZagSubstitutions class >> initialize [

	substitutionMap := self substitutionMap
]

{ #category : 'as yet unclassified' }
ZagSubstitutions class >> substitutionMap [
 " self substitutionMap "

	| map |
	map := IdentityDictionary new.
	self subclassesDo: [ :class | class addMappingsTo: map ].
	^ map
]
