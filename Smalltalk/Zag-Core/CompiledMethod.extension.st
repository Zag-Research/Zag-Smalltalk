Extension { #name : 'CompiledMethod' }

{ #category : '*Zag-Core' }
CompiledMethod >> astFields [
	self halt.
	^ {  }
]

{ #category : '*Zag-Core' }
CompiledMethod >> astWith: anASPharoCompiler [ 
	^ anASPharoCompiler compileMethod: self
]

{ #category : '*Zag-Core' }
CompiledMethod class >> canonicalArgumentName [

	^ 'aCompileMethod'
]
