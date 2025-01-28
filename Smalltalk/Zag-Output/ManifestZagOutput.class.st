"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagOutput',
	#superclass : 'PackageManifest',
	#category : 'Zag-Output-Manifest',
	#package : 'Zag-Output',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagOutput class >> ruleSelfSentNotImplementedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASImageOutput)) #'2025-01-28T07:03:09.665533-05:00') )
]
