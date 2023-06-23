Class {
	#name : #ASPharoCompilerProgramTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #compiling }
ASPharoCompilerProgramTest >> buildFromClass: class selector: method [

	^ ASPharoCompiler new buildFromClass: class selector: method
]

{ #category : #script }
ASPharoCompilerProgramTest >> codeStart42 [
	<script>
	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	^ result zigString
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testAssignment [

	| result aStream |
	result := self
		          buildFromClass: ASCompileTestClass1
		          selector: #testAssignment.

	aStream := (FileSystem disk workingDirectory
	            / 'zigAssignmentString.zig') writeStream.
	result writeZig: aStream.
	aStream close
]

{ #category : #script }
ASPharoCompilerProgramTest >> testCodeStart42 [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	
	(FileSystem disk workingDirectory / 'zigString.txt') writeStream
	nextPutAll: result zigString;
	close.
	
	self assert: result zigString equals: ''
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testFib [
	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #fib.
	
	(FileSystem disk workingDirectory / 'zigFibString.txt') writeStream
	nextPutAll: result zigString;
	close.
	
	self assert: result zigString equals: ''
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntFib [

	| result aStream |
	result := self buildFromClass: Integer selector: #fibonacci.


	aStream := (FileSystem disk workingDirectory / 'zigIntFibString.zig')
		           writeStream.
	result writeZig: aStream.
	aStream close
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testIntbitShiftMagnitude [
	| result aStream |
	result := self
		          buildFromClass: Integer class
		          selector: #bitShiftMagnitude:.
	

	aStream := (FileSystem disk workingDirectory / 'zigBitShiftMagnitudeString.zig') writeStream.
	result writeZig: aStream.
	aStream close.
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testStart42 [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	self assert: result classes equals: { 
			ASTObject.
			ASTBehavior.
			ASTClass.
			ASTClassDescription.
			ASTSmallInteger.
			ASTInteger.
			ASTNumber.
			ASTMagnitude.
			ASTTrue.
			ASTFalse.
			ASTBoolean.
			ASTBlockClosure.
			ASTString.
			ASTArray.
			ASTArrayedCollection.
			ASTSequenceableCollection.
			ASTCollection.
			ASCompileTestClass1 } asSet.
	self
		assert: result selectors
		equals: #(/ do: bitOr: negated + perform:withArguments: numArgsError: minVal
digitAt: new:neg: new: new or: digitCompare: digitLogic:op:length:
copyReplaceFrom:to:with: > cull: superclass isKindOf: primitiveFailed
basicNew whileFalse: and: perform:with: at: copyto: digitSubtract:
error: basicNew: * digitDiv:neg: start42 & isEmpty
digitRshift:bytes:lookfirst: adaptToInteger:andSend: errorSubscriptBounds:
'~~' numArgs normalize instSpec min: errorImproperStore negative
isInteger quo: highBit basicAt: = species asInteger first whileTrue:
value digitLength digitAdd: cannotInstantiate '//' ifNil:ifNotNil:
asCharacter '\\' emptyCheck errorEmptyCollection value: -
replaceFrom:to:with:startingAt: ifFalse: errorNonIntegerIndex
shouldNotImplement bitShift: growby: bitAnd: subclassResponsibility floor
isSmallInteger class add: < with: compare:with:collated: '>=' size '<='
growto: max: not '==' , ifTrue: bitInvert ifTrue:ifFalse: printOn:
printString digitLshift: inheritsFrom: isVariable digitMultiply:neg:
digitAt:put: abs basicSize with:with: bitClear: basicAt:put: initialize
last to:do: isNumber '~=' truncated name lastDigit at:put:) asSet.
	result inspect
]
