"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagCoreTests',
	#superclass : 'PackageManifest',
	#category : 'Zag-Core-Tests-Manifest',
	#package : 'Zag-Core-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleBadMessageRule2V1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASTimings class' #updateBenchmarks #true)) #'2022-08-25T20:02:32.671305+02:00') #(#(#RGMethodDefinition #(#ASemanticInterpreter #interpret #false)) #'2024-08-29T09:17:23.777197-04:00') #(#(#RGMethodDefinition #(#ASCPrimitive2 #interpretFor: #false)) #'2024-11-16T10:20:12.238148-05:00') #(#(#RGMethodDefinition #(#ASCPrimitive1 #interpretFor: #false)) #'2024-11-16T10:20:28.724663-05:00') #(#(#RGMethodDefinition #(#ASCPrimitive #interpretFor: #false)) #'2024-11-16T10:20:34.696857-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleBooleanPrecedenceRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASTimings #dispatch3b #false)) #'2022-08-25T23:29:40.915297+02:00') #(#(#RGMethodDefinition #(#ASTimings #dispatch3 #false)) #'2022-08-30T18:32:01.571322-03:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-04-18T15:12:51.324071-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T20:15:44.342832-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleCollectionProtocolRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-03-19T08:33:58.800965-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleConsistencyCheckRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-03-19T08:34:06.317717-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleExcessiveVariablesRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASemanticInterpreter)) #'2024-12-14T11:47:57.833644-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T19:49:45.328717-05:00') #(#(#RGMethodDefinition #(#ASExploreHashing #min4Table: #false)) #'2021-03-01T08:53:00.023992-05:00') #(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-03-19T08:35:39.276189-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleNobodyShouldSendMethodDictV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-03-19T08:34:13.150996-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> rulePrecedenceRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-04-21T00:02:16.426567-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleReCollectionAtCollectionSizeRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASIStackHeapObjectTest #testSimple #false)) #'2024-11-10T09:39:04.439243-05:00') #(#(#RGMethodDefinition #(#ASemanticInterpreterTest #testSimple #false)) #'2024-11-11T17:55:28.755883-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2024-11-28T19:23:31.208843-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleReNoPrintStringInPrintOnRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASFindPrimitives #printOn: #false)) #'2023-07-09T10:39:44.76397-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleReTemporaryVariableCapitalizationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T19:49:38.367969-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleReVariableAssignedLiteralRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASImageTest)) #'2021-04-23T21:16:23.052204-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'ASImageExporter class' #test #true)) #'2024-11-30T11:48:49.078941-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleSendsUnknownMessageToGlobalRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASPharoCompilerTest)) #'2022-05-26T11:58:53.101937-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASPharoCompilerTest)) #'2022-05-26T11:59:00.854075-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-04-19T09:17:06.696728-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-04-20T18:11:07.482607-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ASMessageStats class' #gen:types: #true)) #'2021-07-07T17:27:13.451371-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleUnclassifiedMethodsRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'Zag-Core-Tests')) #'2021-07-06T19:53:44.153899-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleUsesAddRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASemanticOutput)) #'2024-10-18T16:26:11.139019-04:00') )
]

{ #category : 'code-critics' }
ManifestZagCoreTests class >> ruleUsesTrueRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASCValueTest #testBoolean #false)) #'2023-10-06T18:22:13.148315-04:00') #(#(#RGMetaclassDefinition #(#'ASCompilerInlineTest class' #ASCompilerInlineTest)) #'2024-06-12T11:39:44.923785-04:00') #(#(#RGMethodDefinition #(#ASemanticInterpreter #send:to: #false)) #'2024-08-28T18:02:38.499126-04:00') #(#(#RGMethodDefinition #(#ASemanticInterpreter #initialize #false)) #'2024-08-28T18:43:43.973277-04:00') #(#(#RGMethodDefinition #(#ASemanticInterpreter #reset #false)) #'2024-08-29T08:55:59.601569-04:00') #(#(#RGMethodDefinition #(#ASCBasicBlockTest #setUp #false)) #'2024-10-02T08:44:43.656369-04:00') #(#(#RGMethodDefinition #(#ASCBasicBlockTest #setup8graph #false)) #'2024-10-02T19:48:51.455346-04:00') )
]
