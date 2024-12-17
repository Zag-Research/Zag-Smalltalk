"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestBaselineOfZagSmalltalk',
	#superclass : 'PackageManifest',
	#category : 'BaselineOfZagSmalltalk-Manifest',
	#package : 'BaselineOfZagSmalltalk',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestBaselineOfZagSmalltalk class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#BaselineOfZagSmalltalk #baseline: #false)) #'2024-12-11T18:39:55.723827-05:00') )
]
