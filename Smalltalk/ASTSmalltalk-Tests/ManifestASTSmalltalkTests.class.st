"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestASTSmalltalkTests,
	#superclass : #PackageManifest,
	#category : #'ASTSmalltalk-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleClassNotReferencedRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-04-18T15:12:51.324071-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleCodeCruftLeftInMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T20:15:44.342832-05:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleCollectionProtocolRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-03-19T08:33:58.800965-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleConsistencyCheckRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-03-19T08:34:06.317717-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T19:49:45.328717-05:00') #(#(#RGMethodDefinition #(#ASExploreHashing #min4Table: #false)) #'2021-03-01T08:53:00.023992-05:00') #(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-03-19T08:35:39.276189-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleNobodyShouldSendMethodDictV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-03-19T08:34:13.150996-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> rulePrecedenceRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-04-21T00:02:16.426567-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleReTemporaryVariableCapitalizationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T19:49:38.367969-05:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleReVariableAssignedLiteralRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASImageTest)) #'2021-04-23T21:16:23.052204-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleSendsUnknownMessageToGlobalRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASPharoCompilerTest)) #'2022-05-26T11:58:53.101937-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleSentNotImplementedRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ASPharoCompilerTest)) #'2022-05-26T11:59:00.854075-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-04-19T09:17:06.696728-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-04-20T18:11:07.482607-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#'ASMessageStats class' #gen:types: #true)) #'2021-07-07T17:27:13.451371-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalkTests class >> ruleUnclassifiedMethodsRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#'ASTSmalltalk-Tests')) #'2021-07-06T19:53:44.153899-04:00') )
]
