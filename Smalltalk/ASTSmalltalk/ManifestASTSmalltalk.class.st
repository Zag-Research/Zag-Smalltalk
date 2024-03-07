"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestASTSmalltalk',
	#superclass : 'PackageManifest',
	#category : 'ASTSmalltalk-Manifest',
	#package : 'ASTSmalltalk',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASGenerator #processOn: #false)) #'2021-03-28T20:18:32.15206-04:00') #(#(#RGMethodDefinition #(#ASGenerator #writeFieldsOn: #false)) #'2021-03-28T22:55:47.987048-04:00') #(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:00:23.089715-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleBooleanPrecedenceRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object:fields: #false)) #'2021-04-24T08:09:44.248594-04:00') #(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-04-24T08:13:10.250271-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleCollectionProtocolRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASSourceCompiler #compileMethodInClass: #false)) #'2021-10-09T09:13:12.016388-04:00') #(#(#RGMethodDefinition #(#ASArray #baseValue #false)) #'2023-10-19T10:44:08.600293-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2022-05-16T09:50:39.5032-03:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleExcessiveVariablesRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASCodeGenerator)) #'2023-10-08T10:43:40.424497-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleInstVarInSubclassesRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASStack)) #'2023-10-13T15:05:01.498226-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-03-28T13:19:24.088808-04:00') #(#(#RGMethodDefinition #(#ASSourceCompiler #sequenceTo:then:withReturn: #false)) #'2021-10-04T23:01:49.526703-04:00') #(#(#RGMethodDefinition #(#ASScanner #token #false)) #'2021-10-09T08:06:50.250593-04:00') #(#(#RGMethodDefinition #(#ASCValue #value: #false)) #'2023-10-06T17:33:21.190789-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleMinMaxRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASTThreadFormat #temporaryNames #false)) #'2022-08-30T18:32:18.834631-03:00') #(#(#RGMethodDefinition #(#ASCValue #value: #false)) #'2023-10-06T17:33:14.213283-04:00') #(#(#RGMethodDefinition #(#ASCValue #store: #false)) #'2023-10-06T21:44:23.963698-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleMissingSubclassResponsibilityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASStack)) #'2023-10-13T15:04:56.29713-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleRefersToClassRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(  )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleReturnsIfTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCBlock #manifest: #false)) #'2023-10-19T09:26:04.573515-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASImage class' #image #true)) #'2024-03-07T10:18:00.615661-05:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-03-28T13:56:56.834147-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-03-28T12:35:33.094018-04:00') #(#(#RGMethodDefinition #(#ASMethodNode #zigWalk: #false)) #'2023-10-09T20:35:17.831877-04:00') #(#(#RGMethodDefinition #(#ASMethodNode #startMethod: #false)) #'2023-11-09T11:28:26.132134-05:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:00:13.221856-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleUsesAddRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCodeGenerator #send:targetClass:tail: #false)) #'2023-11-11T11:51:23.341686-05:00') #(#(#RGMethodDefinition #(#ASCBasicBlock #put: #false)) #'2023-11-11T18:07:39.377391-05:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleUsesTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASImage #initialize #false)) #'2021-03-27T10:08:23.326943-04:00') #(#(#RGMethodDefinition #(#ASCompiler #initialize #false)) #'2022-05-18T13:44:20.149973-03:00') #(#(#RGMethodDefinition #(#ASImage #initClasses #false)) #'2024-03-07T08:26:01.023185-05:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #writeSizeOf:on: #false)) #'2021-03-26T17:15:42.158959-04:00') #(#(#RGMethodDefinition #(#ASImage #literal:class: #false)) #'2021-03-26T17:35:53.311307-04:00') #(#(#RGMethodDefinition #(#ASImage #object: #false)) #'2021-03-27T10:43:41.041614-04:00') #(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:11:04.477962-04:00') )
]

{ #category : 'code-critics' }
ManifestASTSmalltalk class >> ruleWhileTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCBasicBlock #send:with: #false)) #'2023-10-08T21:11:13.372871-04:00') #(#(#RGMethodDefinition #(#ASCBasicBlock #send:with:target:tail: #false)) #'2023-10-11T16:50:50.787269-04:00') #(#(#RGMethodDefinition #(#ASCodeGenerator #send:targetClass:tail: #false)) #'2023-10-18T13:16:02.446368-04:00') )
]
