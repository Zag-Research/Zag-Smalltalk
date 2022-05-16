"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestASTSmalltalk,
	#superclass : #PackageManifest,
	#category : #'ASTSmalltalk-Manifest'
}

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleBadMessageRule2V1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASGenerator #processOn: #false)) #'2021-03-28T20:18:32.15206-04:00') #(#(#RGMethodDefinition #(#ASGenerator #writeFieldsOn: #false)) #'2021-03-28T22:55:47.987048-04:00') #(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:00:23.089715-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleBooleanPrecedenceRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object:fields: #false)) #'2021-04-24T08:09:44.248594-04:00') #(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-04-24T08:13:10.250271-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleCollectionProtocolRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASSourceCompiler #compileMethodInClass: #false)) #'2021-10-09T09:13:12.016388-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2022-05-16T09:50:39.5032-03:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleLongMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-03-28T13:19:24.088808-04:00') #(#(#RGMethodDefinition #(#ASSourceCompiler #sequenceTo:then:withReturn: #false)) #'2021-10-04T23:01:49.526703-04:00') #(#(#RGMethodDefinition #(#ASScanner #token #false)) #'2021-10-09T08:06:50.250593-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-03-28T13:56:56.834147-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #include:object: #false)) #'2021-03-28T12:35:33.094018-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleToDoCollectRuleV1FalsePositive [
	^ #(#(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:00:13.221856-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleUsesTrueRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #initialize #false)) #'2021-03-27T10:08:23.326943-04:00') )
]

{ #category : #'code-critics' }
ManifestASTSmalltalk class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ASImage #writeSizeOf:on: #false)) #'2021-03-26T17:15:42.158959-04:00') #(#(#RGMethodDefinition #(#ASImage #literal:class: #false)) #'2021-03-26T17:35:53.311307-04:00') #(#(#RGMethodDefinition #(#ASImage #object: #false)) #'2021-03-27T10:43:41.041614-04:00') #(#(#RGPackageDefinition #(#ASTSmalltalk)) #'2021-04-23T13:11:04.477962-04:00') )
]
