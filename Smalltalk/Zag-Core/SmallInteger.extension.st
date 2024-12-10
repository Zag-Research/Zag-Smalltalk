Extension { #name : 'SmallInteger' }

{ #category : '*Zag-Core' }
SmallInteger >> asZagLiteral [
	^ 'Object.from(', self asString, ')'
]

{ #category : '*Zag-Core' }
SmallInteger >> asZigLiteral [

	^ 'Object.from(' , self asString , ')'
]

{ #category : '*Zag-Core' }
SmallInteger >> astImageObject: anASImage [

	^ anASImage objectSmallInteger: self
]

{ #category : '*Zag-Core' }
SmallInteger >> astObject: anASImage [ 
	^ self
]

{ #category : '*Zag-Core' }
SmallInteger >> fib [ 
	self <= 2 ifTrue: [^ 1].
	^ (self - 1) fib + (self - 2) fib
]

{ #category : '*Zag-Core' }
SmallInteger >> isNonLocalThunkable [

	^ self between: -1 and: 2
]

{ #category : '*Zag-Core' }
SmallInteger >> withIndex: anIndex [
	(anIndex == 1) ifTrue: [ ^ self asString ] ifFalse: [ ^ ' + ( ', self asString, ' << ', (12* (anIndex-1)) asString, ' )' ]

	
]
