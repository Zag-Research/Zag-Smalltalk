"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZagExporterTests',
	#superclass : 'PackageManifest',
	#category : 'Zag-Exporter-Tests-Manifest',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZagExporterTests class >> ruleReCyclomaticComplexityRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ASZagImageTest #testSimple #false)) #'2024-12-03T20:06:45.52703-05:00') )
]
