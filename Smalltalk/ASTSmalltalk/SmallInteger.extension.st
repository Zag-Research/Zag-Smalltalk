Extension { #name : 'SmallInteger' }

{ #category : '*ASTSmalltalk' }
SmallInteger >> asZagLiteral [
	^ 'Object.from(', self asString, ')'
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> asZigLiteral [

	^ 'Object.from(' , self asString , ')'
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> astImageObject: anASImage [

	^ anASImage putSmallInteger: self
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> astObject: anASImage [ 
	^ self
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> isNonLocalThunkable [

	^ self between: -1 and: 2
]
