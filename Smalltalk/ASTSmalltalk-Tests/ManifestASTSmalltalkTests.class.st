"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestASTSmalltalkTests,
	#superclass : #PackageManifest,
	#category : #'ASTSmalltalk-Tests-Manifest'
}

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
ManifestASTSmalltalkTests class >> ruleReTemporaryVariableCapitalizationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASExploreHashing #crtTable: #false)) #'2021-02-28T19:49:38.367969-05:00') )
]
