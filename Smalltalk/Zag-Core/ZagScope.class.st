"
I am the scope for instance, class, pool, and global variables
"
Class {
	#name : 'ZagScope',
	#superclass : 'Object',
	#instVars : [
		'class'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ZagScope >> at: aSymbol [

	^ ZagInstvar new index: (class instVarNames indexOf: aSymbol ifAbsent: [ ^ self tryClassVariable: aSymbol ])
]

{ #category : 'instance creation' }
ZagScope >> class: aClass [

	class := aClass
]
