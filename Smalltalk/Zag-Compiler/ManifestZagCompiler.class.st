"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagCompiler',
	#superclass : 'PackageManifest',
	#category : 'Zag-Compiler-Manifest',
	#package : 'Zag-Compiler',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagCompiler class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASScanner #token #false)) #'2026-01-18T10:38:45.458102-05:00') #(#(#RGMethodDefinition #(#ASScanner #possibleSeparator #false)) #'2026-01-29T08:13:17.604452-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCompiler class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASScanner #token #false)) #'2026-01-18T10:38:40.515933-05:00') #(#(#RGMethodDefinition #(#ASScanner #possibleSeparator #false)) #'2026-01-29T08:00:34.211457-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCompiler class >> ruleReStatementsAfterReturnConditionalRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASScanner #token #false)) #'2026-01-18T10:40:02.556338-05:00') )
]

{ #category : 'code-critics' }
ManifestZagCompiler class >> ruleSearchingLiteralRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASScanner #possibleSeparator #false)) #'2026-01-29T08:00:30.597867-05:00') )
]
