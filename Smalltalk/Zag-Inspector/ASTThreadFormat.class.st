"
I format an IRMethod to generate Zig code to create a method thread.

The Opal compiler changed the classes I extend, so the following method has to be run class-side for `OCIRInstruction`:
```smalltalk
makeDeprecatedClassAliases

    <script>
    self withAllSubclassesDo: [ :class |
        | deprecatedClass |
        deprecatedClass := (class name allButFirst: 2) asSymbol.
        class deprecatedAliases: { deprecatedClass } ]
```
Similarly for `OCIRSequence` and maybe for `OCIRMethod` (though that one is weird as of 2025.01.31):
```smalltalk
makeDeprecatedClassAlias

    <script>
    | deprecatedClass |
    deprecatedClass := (self name allButFirst: 2) asSymbol.
    self deprecatedAliases: { deprecatedClass } ]
```

"
Class {
	#name : 'ASTThreadFormat',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'method'
	],
	#category : 'Zag-Inspector-Inspect',
	#package : 'Zag-Inspector',
	#tag : 'Inspect'
}

{ #category : 'printing' }
ASTThreadFormat >> cr [
	^ stream cr
]

{ #category : 'tests' }
ASTThreadFormat >> loop [

	| x |
	x := 1000000000.
	[ (x := x - 1) > 0 ] whileTrue: [ 
		x even
			ifTrue: [ x * 2 + 3 ]
			ifFalse: [ x * 3 + 1 ] ]
]

{ #category : 'printing' }
ASTThreadFormat >> nextPut: aCharacter [ 
	^ stream nextPut: aCharacter
]

{ #category : 'printing' }
ASTThreadFormat >> nextPutAll: aString [ 
	^ stream nextPutAll: aString
]

{ #category : 'printing' }
ASTThreadFormat >> printTempOffset: aSymbol [
	stream nextPutAll: (method tempMap at: aSymbol) asString
]

{ #category : 'accessing' }
ASTThreadFormat >> temporaryNames [
	| x |
	x := 42.
	^ x<0 ifTrue: [ 0 ] ifFalse: [ x ]
]
