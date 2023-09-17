Class {
	#name : #ASPharoCompilerObjectTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASPharoCompilerObjectTest >> testCompileMethodToObject [
	"
	 + aNumber 
         ""Primitive. Add the receiver to the argument and answer with the result
         if it is a SmallInteger. Fail if the argument or the result is not a
         SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive.""

         <primitive: 1>
         ^ super + aNumber
"

	| compiler result aStream |
	compiler := ASPharoCompiler new
		            buildFromClassShallow: SmallInteger
		            selector: SmallInteger >> #+.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') writeStream.
	result do: [ :line | aStream << line. aStream cr.].
	aStream close
]
