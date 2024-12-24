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
	^ #(#(#(#RGMethodDefinition #(#ASZagImageTest #testSimple #false)) #'2024-12-03T20:06:45.52703-05:00') #(#(#RGMethodDefinition #(#ASNanImageTest #testSimple #false)) #'2024-12-06T01:10:55.974704-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporterTests class >> ruleRefersToClassRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#ASImageTest)) #'2024-12-23T17:03:45.221268-05:00') )
]

{ #category : 'code-critics' }
ManifestZagExporterTests class >> ruleSendsDifferentSuperRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zag-Exporter-Tests')) #'2024-12-23T17:10:53.079304-05:00') )
]
