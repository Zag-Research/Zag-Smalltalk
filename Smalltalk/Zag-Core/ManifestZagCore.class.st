"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagCore',
	#superclass : 'PackageManifest',
	#category : 'Zag-Core-Manifest',
	#package : 'Zag-Core',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagCore class >> ruleAssignmentInBlockRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #ASCodeGenerator #inlineBlock:next:
	              #false ) )
	        #'2024-06-01T00:14:10.797886-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleBadMessageRule2V1FalsePositive [

	^ #( #( #( #RGPackageDefinition #( #'Zag-Core' ) )
	        #'2021-04-23T13:00:23.089715-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleCollectionProtocolRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #ASSourceCompiler #compileMethodInClass:
	              #false ) )
	        #'2021-10-09T09:13:12.016388-04:00' )
	     #( #( #RGMethodDefinition #( #ASArray #baseValue #false ) )
	        #'2023-10-19T10:44:08.600293-04:00' )
	     #( #( #RGMethodDefinition #( #ASLLMethodOutput #setupForOutput
	              #false ) )
	        #'2024-05-26T22:42:46.085628-07:00' )
	     #( #( #RGMethodDefinition #( #ASCodeGenerator #inlineMultiple:from:exhaustive:
	              #false ) )
	        #'2024-07-07T18:33:46.333081+02:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleConsistencyCheckRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASSubHeap #prepareForOutput #false)) #'2024-11-30T00:08:38.138311-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core')) #'2022-05-16T09:50:39.5032-03:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleExcessiveMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASCBasicBlock)) #'2024-06-12T20:22:50.927097-04:00') #(#(#RGClassDefinition #(#ASCodeGenerator)) #'2024-06-25T09:46:18.241926-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleExcessiveVariablesRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGClassDefinition #( #ASCodeGenerator ) )
	        #'2023-10-08T10:43:40.424497-04:00' )
	     #( #( #RGClassDefinition #( #ASCBasicBlock ) )
	        #'2024-06-22T13:01:37.160642-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleInstVarInSubclassesRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASStack)) #'2023-10-13T15:05:01.498226-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #ASSourceCompiler #sequenceTo:then:withReturn:
	              #false ) )
	        #'2021-10-04T23:01:49.526703-04:00' )
	     #( #( #RGMethodDefinition #( #ASScanner #token #false ) )
	        #'2021-10-09T08:06:50.250593-04:00' )
	     #( #( #RGMethodDefinition #( #AStExactClass #value: #false ) )
	        #'2023-10-06T17:33:21.190789-04:00' )
	     #( #( #RGMethodDefinition #( #'ASImage class' #initialize
	              #true ) )
	        #'2024-04-12T11:41:16.943424-04:00' )
	     #( #( #RGMethodDefinition #( #'ASAllocationInfo class'
	              #headerIVars:indexed:element:weak:
	              #true ) )
	        #'2024-04-13T20:36:41.511805-04:00' )
	     #( #( #RGMethodDefinition #( #ASCodeGenerator #initialize
	              #false ) )
	        #'2024-06-28T09:13:23.111016-04:00' )
	     #( #( #RGMethodDefinition #( #'ASInliner class'
	              #reset #true ) )
	        #'2024-08-10T09:35:19.414618-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleMinMaxRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASTThreadFormat #temporaryNames #false)) #'2022-08-30T18:32:18.834631-03:00') #(#(#RGMethodDefinition #(#AStExactClass #value: #false)) #'2023-10-06T17:33:14.213283-04:00') #(#(#RGMethodDefinition #(#AStExactClass #store: #false)) #'2023-10-06T21:44:23.963698-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleMissingSubclassResponsibilityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASStack)) #'2023-10-13T15:04:56.29713-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleReCollectionAtCollectionSizeRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASSpurPlusImage class' #double:for: #true)) #'2024-11-27T22:26:37.421275-05:00') #(#(#RGMethodDefinition #(#'ASZagImageExporter class' #double:for: #true)) #'2024-11-30T11:49:44.395611-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASAllocationInfo class' #headerIVars:indexed:element:weak: #true)) #'2024-11-30T09:16:55.515465-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleRefersToClassRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #AStSmallInteger #couldMergeIn:
	              #false ) )
	        #'2024-09-22T08:44:00.560578-04:00' )
	     #( #( #RGMethodDefinition #( #AStSmallInteger #mergeIn:
	              #false ) )
	        #'2024-09-22T09:16:54.776207-04:00' )
	     #( #( #RGMethodDefinition #( #'ZagImageEncoder class'
	              #from: #true ) )
	        #'2024-12-03T19:53:22.465308-05:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASImage class' #test #true)) #'2024-04-12T11:43:11.618971-04:00') #(#(#RGMethodDefinition #(#'ASImage class' #image #true)) #'2024-04-12T11:44:19.44855-04:00') #(#(#RGMethodDefinition #(#'ASImageExporter class' #test #true)) #'2024-11-28T15:20:26.851998-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core')) #'2021-03-28T13:56:56.834147-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASMethodNode #zigWalk: #false)) #'2023-10-09T20:35:17.831877-04:00') #(#(#RGMethodDefinition #(#ASMethodNode #startMethod: #false)) #'2023-11-09T11:28:26.132134-05:00') #(#(#RGMethodDefinition #(#ASFlowPhi #dataClass #false)) #'2024-07-08T22:55:17.389871+02:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core')) #'2021-04-23T13:00:13.221856-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleUnaryAccessingMethodWithoutReturnRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASStackTemporary #nameReset #false)) #'2024-05-13T08:20:53.254599-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleUnwindBlocksRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #ASCodeGenerator #inlineBlock:next:
	              #false ) )
	        #'2024-06-01T00:13:55.45785-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleUsesAddRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCodeGenerator #send:targetClass:tail: #false)) #'2023-11-11T11:51:23.341686-05:00') #(#(#RGMethodDefinition #(#ASCBasicBlock #addOperation: #false)) #'2023-11-11T18:07:39.377391-05:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #newInline #false)) #'2024-06-18T13:33:21.771718-04:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #newInlineBlockClosure #false)) #'2024-06-23T11:10:01.368119-04:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #newInlinedBlock: #false)) #'2024-10-18T21:33:11.841325-04:00') #(#(#RGMethodDefinition #(#AStackLocation #consumer: #false)) #'2024-10-29T08:47:55.259044-04:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #primitive: #false)) #'2024-11-15T10:07:37.092192-05:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #addBlock: #false)) #'2024-11-15T11:36:57.73124-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleUsesTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #( #( #( #RGMethodDefinition #( #ASCompiler #initialize #false ) )
	        #'2022-05-18T13:44:20.149973-03:00' )
	     #( #( #RGMethodDefinition #( #'ASImage class' #initialize
	              #true ) )
	        #'2024-04-12T11:41:20.538627-04:00' )
	     #( #( #RGMethodDefinition #( #ASCodeGenerator #initialize
	              #false ) )
	        #'2024-06-28T09:13:18.549021-04:00' )
	     #( #( #RGMethodDefinition #( #ASInliner #initialize #false ) )
	        #'2024-07-03T08:54:00.244969-04:00' )
	     #( #( #RGMethodDefinition #( #'ASInliner class'
	              #lookup:stack: #true ) )
	        #'2024-07-04T12:20:52.814202-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleUtilityMethodsRuleV1FalsePositive [

	^ #(  #( #( #RGPackageDefinition #( #'Zag-Core' ) )
	        #'2021-04-23T13:11:04.477962-04:00' ) )
]

{ #category : 'code-critics' }
ManifestZagCore class >> ruleWhileTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCBasicBlock #send:with: #false)) #'2023-10-08T21:11:13.372871-04:00') #(#(#RGMethodDefinition #(#ASCBasicBlock #send:with:target:tail: #false)) #'2023-10-11T16:50:50.787269-04:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #send:targetClass:tail: #false)) #'2023-10-18T13:16:02.446368-04:00') )
]
