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

	| compiler result aStream aReader lines astTree |
	compiler := ASPharoCompiler new
		            buildFromClass: SmallInteger
		            method: SmallInteger >> #+.
	result := compiler asCompileObject.

	self assert: nil equals: nil.
	aStream := (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') writeStream.
	aStream truncate.
	result do: [ :line | aStream << line. aStream cr.].
	aStream close.
	
	aReader :=  (FileSystem disk workingDirectory
	            / 'zigSmallIntegerAddAsCompileObject.zig') readStream.
	lines := OrderedCollection new.
	[ aReader atEnd ] whileFalse: [ lines add: aReader nextLine].
	aReader close.
	
	astTree := 	compiler asAST: lines.
]
