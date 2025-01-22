"
I represent a single method to be inlined.
That is, I know that this is the only possible implementation because the target class is known.
"
Class {
	#name : 'ASInlineSingleton',
	#superclass : 'ASInline',
	#instVars : [
		'method',
		'targetClass'
	],
	#category : 'Zag-Inlining-Inline',
	#package : 'Zag-Inlining',
	#tag : 'Inline'
}

{ #category : 'initialization' }
ASInlineSingleton class >> method: aCompiledMethod class: aClass [
	| method |
	method := aCompiledMethod.
	ZagSubstitutions substitutionMap at: aCompiledMethod ifPresent: [ :m | method := m ].
	
	^ (method isRealPrimitive
		   ifTrue: [ ASInlinePrimitive ]
		   ifFalse: [ self ]) new method: method class: aClass
]

{ #category : 'inlining' }
ASInlineSingleton >> inline: anASCSend with: anASCodeGenerator [

	^ anASCodeGenerator
		  inlineMethod: method
		  for: targetClass
		  from: anASCSend
]

{ #category : 'accessing' }
ASInlineSingleton >> method [
	^ method
]

{ #category : 'initialization' }
ASInlineSingleton >> method: aCompiledMethod class: aClass [

	method := aCompiledMethod.
	targetClass := aClass
]

{ #category : 'testing' }
ASInlineSingleton >> validate: aStackElement [

	^ self
]
