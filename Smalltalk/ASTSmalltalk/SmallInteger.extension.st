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
SmallInteger >> fib [ 
	self <= 2 ifTrue: [^ 1].
	^ (self - 1) fib + (self - 2) fib
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> fibonacci [ 
	self <= 2 ifTrue: [^ 1].
	^ (self - 1) fib + (self - 2) fib
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> isNonLocalThunkable [

	^ self between: -1 and: 2
]

{ #category : '*ASTSmalltalk' }
SmallInteger >> withIndex: anIndex [
	(anIndex == 1) ifTrue: [ ^ self asString ] ifFalse: [ ^ ' + ( ', self asString, ' << ', (12* (anIndex-1)) asString, ' )' ]

	
]
