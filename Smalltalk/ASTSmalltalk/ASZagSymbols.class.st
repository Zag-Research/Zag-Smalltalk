"
I manage the values to go into a zag symbols table. I have predefined symbols and runtime symbols.
"
Class {
	#name : 'ASZagSymbols',
	#superclass : 'Object',
	#instVars : [
		'runtimeDefined'
	],
	#classVars : [
		'Predefined'
	],
	#category : 'ASTSmalltalk-Contexts',
	#package : 'ASTSmalltalk',
	#tag : 'Contexts'
}

{ #category : 'as yet unclassified' }
ASZagSymbols class >> initialize [
	"in future better to load sysmbols directly from symbol.zig, may need a config file so we know where symbol.zig is"

	Predefined := #( yourself doesNotUnderstand: #= #+ #- #* size at: at:put: #'~='
	   #'==' #'~~' value value: value:value: negated new new: cull: value:value:value:
	   value:value:value:value: valueWithArguments: cull:cull: cull:cull:cull:
	   cull:cull:cull:cull: self name #< #'<=' #'>=' #> class Class Behavior
	   ClassDescription Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:perform:withArguments:
	   perform:withArguments:inSuperclass:
	   #'\\' whileTrue: Object ) asSet
]

{ #category : 'as yet unclassified' }
ASZagSymbols >> add: aSymbol [

	runtimeDefined add: aSymbol
]

{ #category : 'as yet unclassified' }
ASZagSymbols >> initialize [ 
	super initialize .
	runtimeDefined := Set new
]
