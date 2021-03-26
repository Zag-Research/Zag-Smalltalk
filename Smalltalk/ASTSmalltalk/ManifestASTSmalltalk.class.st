"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestASTSmalltalk,
	#superclass : #PackageManifest,
	#category : #'ASTSmalltalk-Manifest'
}

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #writeSizeOf:on: #false)) #'2021-03-26T17:15:42.158959-04:00') #(#(#RGMethodDefinition #(#ASImage #literal:class: #false)) #'2021-03-26T17:35:53.311307-04:00') )
]
