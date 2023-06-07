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

{ #category : #script }
ASPharoCompilerProgramTest >> testCodeStart42 [

	| result |
	result := self
		          buildFromClass: ASCompileTestClass1 class
		          selector: #start42.
	self assert: result zigString equals: ''
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
			ASTBlock.
			ASTString.
			ASTArray.
			ASTArrayedCollection.
			ASTSequenceableCollection.
			ASTCollection.
			ASCompileTestClass1 } asSet.
	self
		assert: result selectors
		equals: #( at:put: do: negated bitOr: + perform:withArguments: minVal
digitAt: new:neg: new: new or: digitCompare: digitLogic:op:length:
copyReplaceFrom:to:with: > superclass isKindOf: primitiveFailed basicNew
and: perform:with: at: copyto: digitSubtract: error: basicNew: *
digitDiv:neg: start42 isEmpty digitRshift:bytes:lookfirst:
adaptToInteger:andSend: errorSubscriptBounds: '~~' normalize instSpec min:
errorImproperStore negative isInteger quo: highBit basicAt: = species
asInteger first digitLength digitAdd: cannotInstantiate '//'
ifNil:ifNotNil: asCharacter '\\' emptyCheck errorEmptyCollection -
replaceFrom:to:with:startingAt: ifFalse: errorNonIntegerIndex
shouldNotImplement bitShift: growby: floor subclassResponsibility bitAnd:
isSmallInteger class add: < with: compare:with:collated: '>=' size '<='
growto: max: not '==' , ifTrue: bitInvert ifTrue:ifFalse: printOn:
printString digitLshift: inheritsFrom: isVariable digitMultiply:neg:
digitAt:put: abs basicSize with:with: bitClear: basicAt:put: initialize
to:do: isNumber '~=' truncated name lastDigit / ) asSet.
	result inspect
]
