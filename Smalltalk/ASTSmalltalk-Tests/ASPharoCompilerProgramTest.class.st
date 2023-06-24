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
			"ASTBlockClosure. removed temporarily"
			ASTString.
			ASTArray.
			ASTArrayedCollection.
			ASTSequenceableCollection.
			ASTCollection.
			ASCompileTestClass1 } asSet.
	self
		assert: result selectors
		equals: #( at:put: do: negated bitOr: + perform:withArguments:
			   numArgsError: minVal byteAt: new:neg: new: new or: digitCompare:
			   igitLogic:op:length: copyReplaceFrom:to:with:
			   > cull: superclass primitiveFailed isKindOf: basicNew whileFalse:
			   and: perform:with: at: copyto: digitSubtract: error: basicNew:
			   * digitDiv:neg: start42 & isEmpty digitRshift:bytes:lookfirst:
			   adaptToInteger:andSend: ~~ errorSubscriptBounds:
			   numArgs normalize instSpec min: errorImproperStore
			   negative isInteger quo: highBit basicAt: = species asInteger
			   first whileTrue: value bytesCount digitAdd: cannotInstantiate
			   // ifNil:ifNotNil: asCharacter \\ emptyCheck errorEmptyCollection
			   value: - replaceFrom:to:with:startingAt:
			   ifFalse: errorNonIntegerIndex shouldNotImplement
			   bitShift: growby: floor subclassResponsibility
			   bitAnd: isSmallInteger class add: < with: compare:with:collated:
			   >= size <= growto: max: not == , ifTrue: bitInvert ifTrue:ifFalse:
			   printOn: printString digitLshift: inheritsFrom: isVariable
			   digitMultiply:neg: byteAt:put: abs basicSize with:with:
			   bitClear: basicAt:put: initialize last to:do: isNumber
			   ~= truncated name lastDigit / ) asSet.
	result inspect
]
