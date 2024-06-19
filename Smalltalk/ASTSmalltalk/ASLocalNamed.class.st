"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASLocalNamed',
	#superclass : 'ASStackNamed',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'data flow instructions' }
ASLocalNamed >> newElementFor: aBasicBlock [

	| new |
	new := aBasicBlock classForLocals new id: name.
	aBasicBlock addSource: new.
	^ (self class fromStack: (rest newElementFor: aBasicBlock))
		  name: name;
		  value: value;
		  source: new
]
