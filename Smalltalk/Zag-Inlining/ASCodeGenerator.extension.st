Extension { #name : 'ASCodeGenerator' }

{ #category : '*Zag-Inlining' }
ASCodeGenerator >> inlineBlock: offset next: common [

	| savedReturn savedTargetClass blockClosure |
	savedTargetClass := targetClass.
	savedReturn := returnBlock.
	[
	| inlineBlock |
	returnBlock := common.
	inlineBlock := self newInlineBlockClosure.
	code removeLastOperation.
	self branch: inlineBlock.
	code := inlineBlock.
	blockClosure := (code stackBelow: offset) stackValue.
	blockClosures remove: blockClosure.
	inlineBlock branch: returnBlock.
	blockClosure ast zigWalkMethod: self.
	code := returnBlock ] ensure: [
		returnBlock := savedReturn.
		targetClass := savedTargetClass ]
]

{ #category : '*Zag-Inlining' }
ASCodeGenerator >> inlineMethod: aMethodAST for: inlinedTargetClass from: anASCSend [

	| inlineBlock savedTargetClass |
	savedTargetClass := targetClass.
	inlineBlock := self newInlinedBlock: aMethodAST.
	code removeLastOperation.
	inlineBlock copyStackFrom: code.
	[
	targetClass := inlinedTargetClass.
	self branch: inlineBlock.
	returnBlock := self getOrMakeReturnBlock.
	code := inlineBlock.
	(ASPharoCompiler new astFor: aMethodAST) zigWalkMethod: self ]
		ensure: [ "when you hit a return while walking the method you need to not do returnTop you need to return to returnBlock""self can hold the return block"
			code := returnBlock.
			targetClass := savedTargetClass ]
]

{ #category : '*Zag-Inlining' }
ASCodeGenerator >> inlineMultiple: methods from: anASCSend exhaustive: isExhaustive [

	| case joinBlock |
	isExhaustive ifTrue: [
		code removeLastOperation.
		joinBlock := ASCJoinBlock new branch: anASCSend next becomeJoinBlock.
		self addBlock: joinBlock ].
	case := code
		        classCase: (methods collect: #methodClass)
		        selector: methods first selector.
	isExhaustive ifFalse: [
		code swapLastOperations.
		joinBlock := anASCSend next ].
	^ methods do: [ :methodAST |
		  | targetBlock |
		  targetBlock := case at: methodAST methodClass.
		  targetBlock next: joinBlock.
		  self addBlock: targetBlock.
		  self setBasicBlock: targetBlock.
		  "walk the sends method AST"
		  self inlineMethod: methodAST for: targetBlock from: anASCSend ]
]
